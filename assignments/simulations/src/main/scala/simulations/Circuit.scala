package simulations

import common._
import scala.annotation.tailrec

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) = {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig || a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) = {
    val b1, b2, c = new Wire
    
    inverter(a1, b1)
    inverter(a2, b2)
    
    andGate(b1, b2, c)
    
    inverter(c, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) = {
    assert(out.length == math.pow(2, c.length))
    
    //if false, out1, if true, out2
    def ifConnect(input: Wire, switcher: Wire, out1: Wire, out2: Wire) = {
      val notSwitcher = new Wire
      inverter(switcher, notSwitcher)
      andGate(input, switcher, out2)
      andGate(input, notSwitcher, out1)
    }
    
    @tailrec
    def demuxInner(/*ins: List[Wire], */cs: List[Wire], outs: List[Wire]): Unit = {
      cs match {
        case ch :: ct => { //latest switcher
          outs match {
            case o1 :: o0 :: Nil => {
              ifConnect(in, ch, o0, o1)
              Nil
            }
            case _ => {
              val gr = outs.grouped(2)
              val newOuts: List[Wire] = gr.map(ls => ls match {
                case h :: t :: Nil => {
                  val ret = new Wire
                  ifConnect(ret, ch, t, h)
                  ret
                } 
                case _ => throw new Exception("should not be reachable")
              }).toList
              demuxInner(ct, newOuts)
            }
          }
        }
        case _ => Nil 
      }
    }
    demuxInner(c.reverse, out)
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
