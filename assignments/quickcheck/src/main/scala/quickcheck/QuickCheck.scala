package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a:Int, b:Int) =>
    val h = insert(b, insert(a, empty))
    val min = findMin(h)
    (min == a || (a > b && min == b)) 
  }
  
  property("min meld") = forAll { (ah: H, bh: H) =>
    val melded = meld(ah, bh)
    if(!isEmpty(ah) && !isEmpty(bh)) {
      findMin(melded) == findMin(ah) || findMin(melded) == findMin(bh)
    } else {
      true
    }
  }
  
  property("minimal elements are sorted") = forAll { (h: H) =>
    isSorted(h)
  }
  
  property("minimal elements in melded are sorted") = forAll { (ah: H, bh: H) =>
    isSorted(meld(ah,bh))
  }
  
  property("min of a meld is a min of the melded") = forAll { (ah: H, bh: H) =>
    if(!isEmpty(ah) && !isEmpty(bh)) {
      if(findMin(ah) < findMin(bh))
        findMin(meld(ah,bh)) == findMin(ah)
      else 
        findMin(meld(ah,bh)) == findMin(bh) 
    } else {
      true
    }
  }
  
  property("is sorted after delete") = forAll { (h: H) =>
    isEmpty(h) || isSorted(deleteMin(h))
  }
  
  property("meld empty when both empty") = forAll { (ah: H, bh: H) =>
    if(isEmpty(meld(ah, bh))){
      isEmpty(ah) && isEmpty(bh)
    } else {
      true
    }
  }
  
  def isSorted(h: H): Boolean = {
    getAll(h).sliding(2).forall(ls => ls.head <= ls.last)
  }
  
  def isStictlySorted(h: H): Boolean = {
    getAll(h).sliding(2).forall(ls => ls.head < ls.last)
  }
  property("empty on delete") = forAll { a: Int =>
    val single = insert(a, empty)
    isEmpty(deleteMin(single))
  }
  
  def heapsIdentical(h1: H, h2: H): Boolean = {
    val ls1 = getAll(h1) 
    val ls2 = getAll(h2)
    val lengthsEqual = ls1.length == ls2.length
    
    lazy val zippedEqual = ls1.zip(ls2).forall(pr => pr._1 == pr._2)
    lengthsEqual && zippedEqual
  }
  
  property("same heaps identical") = forAll { h: H =>
    heapsIdentical(h, h)
  }
  
  property("differently constructed heaps identical") = forAll { triplet: (Int, Int, Int) =>
    val (a, b, c) = triplet
    val h1 = insert(c, insert(b, insert(a, empty)))
    val h2 = insert(a, insert(b, insert(c, empty)))
    heapsIdentical(h1, h2)
  }
  
  property("delete min deletes the min") = forAll { h: H =>
    if(h != empty && isStictlySorted(h)) {
      val min = findMin(h)
      val rest = deleteMin(h)
      if(!isEmpty(rest)) {
        findMin(rest) != min
      } else {
        true
      }
    } else {
      true
    }
    
  }
  
  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(v, h)
  
  def getAll(h: H): List[A] = {
    def getAllInner(acc: List[Int], h: H): List[Int] = {
      if(isEmpty(h)) {
        acc
      } else {
        getAllInner(findMin(h) :: acc, deleteMin(h))
      }
    }
    getAllInner(Nil, h).reverse
  }
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(Gen.lzy(Gen.oneOf(genHeap, empty)))

}
