/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
//import java.util.SelfComparable

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = { 
    case op: Operation => root ! op;
    case GC => {
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot), false)
      root ! BinaryTreeNode.CopyTo(newRoot)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation => {
      pendingQueue = pendingQueue.enqueue(op)
    }
    case GC => {
      //passive-aggressive ignoring
    }
    case BinaryTreeNode.CopyFinished => {
      context.stop(root)
      root = newRoot
      while(!pendingQueue.isEmpty){
        var tuple = pendingQueue.dequeue
        pendingQueue = tuple._2
        root ! tuple._1
      }
      //pendingQueue.foreach(op => root ! op)
      context.unbecome()
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal
  
  def potentiallyInvolvedChild(targetElem: Int): Option[ActorRef] = {
    if(targetElem < elem) {
      subtrees.get(Left)
    } else if(targetElem > elem) {
      subtrees.get(Right)
    } else {
      None
    }
  }
  
  def addChild(childsElem: Int, initiallyRemoved: Boolean): ActorRef = {
    val childPos: Position =  if(childsElem < elem) Left else Right;
    val ar = context.actorOf(BinaryTreeNode.props(childsElem, initiallyRemoved))
    subtrees += Tuple2(childPos, ar)
    ar
  }
  def copyId: Int = elem //FIXME?
  
  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = { 
    case op: Operation => {
      val pic = potentiallyInvolvedChild(op.elem)
      val thisIsTarget = op.elem == elem
      op match {
        // Insert
        case Insert(requester, id, reqElem) => {
          if(thisIsTarget) {
            this.removed = false;
            requester ! OperationFinished(id)
          } else {
            pic match {
              case None => {
                addChild(op.elem, false)
                requester ! OperationFinished(id)
              }
              case Some(child) => {
                child ! op
              }
            }
          }
        }
        // Remove
        case Remove(requester, id, reqElem) => {
          if(thisIsTarget) {
            this.removed = true;
            requester ! OperationFinished(id)
          } else {
            pic match {
              case None => {
                requester ! OperationFinished(id)
              }
              case Some(child) => {
                child ! op
              }
            }
          }
        }
        // Contains
        case Contains(requester, id, reqElem) => {
          if(thisIsTarget) {
            requester ! ContainsResult(id, !this.removed)
          } else {
            pic match {
              case None => {
                requester ! ContainsResult(id, false)
              } 
              case Some(child) => {
                child ! op
              }
            }
          }
        }
      }
    }
    case CopyTo(target) => {
      //insert myself into the target
      if(!this.removed)
        target ! Insert(context.self, copyId, elem)
      //tell children to copy themselves to the target
      subtrees.values.foreach(child => child ! CopyTo(target))
      //start awaiting for all copying actions to be completed
      if(!subtrees.values.toSet.isEmpty || !this.removed) {
        context.become(copying(subtrees.values.toSet, this.removed), true)
      } else {
        context.parent ! CopyFinished
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) => {
      if (id == copyId) {
        if (expected.isEmpty) {
          //nothing left to do
          context.parent ! CopyFinished
          context.become(normal, true)
        } else {
          //still gotta wait for the children to be copied
          context.become(copying(expected, true), true)
        }
      }
    }
    case CopyFinished => {
      val newExpected = expected - context.sender
      if(insertConfirmed && newExpected.isEmpty) {
        context.parent ! CopyFinished
        context.become(normal, true)
      } else {
        context.become(copying(newExpected, insertConfirmed))
      }
    }
  }

}
