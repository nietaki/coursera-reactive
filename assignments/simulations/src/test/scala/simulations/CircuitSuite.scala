package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
   test("small demux") {
    val in, c, o0, o1 = new Wire
    val cs = c :: Nil
    val o = o1 :: o0 :: Nil
    demux(in, cs, o)

    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)

    in.setSignal(true)
    run
    assert(o0.getSignal === true)
    assert(o1.getSignal === false)
    
    in.setSignal(true)
    c.setSignal(true)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === true)
  } 
 test("demux") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    val c = c1 :: c0 :: Nil
    val o = o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, c, o)

    run
    assert(o3.getSignal === false)
    assert(o2.getSignal === false)
    assert(o1.getSignal === false)
    assert(o0.getSignal === false)

    in.setSignal(true)
    run
    assert(o3.getSignal === false)
    assert(o2.getSignal === false)
    assert(o1.getSignal === false)
    assert(o0.getSignal === true)
   
    in.setSignal(true)
    c0.setSignal(true)
    run
    assert(o3.getSignal === false)
    assert(o2.getSignal === false)
    assert(o1.getSignal === true)
    assert(o0.getSignal === false)
    
    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(true)
    run
    assert(o3.getSignal === true)
    assert(o2.getSignal === false)
    assert(o1.getSignal === false)
    assert(o0.getSignal === false)
  } 
}
