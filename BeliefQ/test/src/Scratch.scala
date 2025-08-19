package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

class ScratchComp extends Component {
  val v = out port AFix.SQ(5 bits, 2 bits)
  val w = AFix.SQ(5 bits, 2 bits)
  w := -BigDecimal("1.5")
  v := (-w).truncated
}

object Scratch extends TestSuite {
  def tests = Tests {
    val params = new BeliefQParams
    test("hello test") {
      SimConfig.compile { new SurfaceCodeDecoder(params, 3, 2) }.doSim { dut =>
        sleep(1)
        assert(dut.test.toBoolean)
      }
    }
    test("scratch") {
      SimConfig.compile { new ScratchComp() }.doSim { dut =>
        sleep(1)
        println(dut.v.toBigDecimal)
      }
    }
  }
}
