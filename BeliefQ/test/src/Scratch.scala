package beliefq
package test

import beliefq.generate._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

class TestComp extends Component {
  val a = in port Bool()
  val aa = out port Bool()
  val b = out port Reg(Bool())
  val c = out port Reg(Bool())
  aa := a
  b := a
  c := b
}

object Scratch extends TestSuite {
  def tests = Tests {
    val params = new BeliefQParams()
    test("hello test") {
      SimConfig.compile { new SurfaceCodeDecoder(params, 3, 2) }.doSim { dut =>
        sleep(1)
        assert(dut.test.toBoolean)
      }
    }
    test("scratch") {
      SimConfig.compile { new TestComp() }.doSim { dut =>
        dut.a #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        cd.waitSampling()
        dut.a #= true
        println(f"a = ${dut.a.toBoolean}; aa = ${dut.aa.toBoolean}, b = ${dut.b.toBoolean}, c = ${dut.c.toBoolean}")
        cd.waitSampling()
        println(f"a = ${dut.a.toBoolean}; aa = ${dut.aa.toBoolean}, b = ${dut.b.toBoolean}, c = ${dut.c.toBoolean}")
        cd.waitSampling()
        println(f"a = ${dut.a.toBoolean}; aa = ${dut.aa.toBoolean}, b = ${dut.b.toBoolean}, c = ${dut.c.toBoolean}")
      }
    }
  }
}
