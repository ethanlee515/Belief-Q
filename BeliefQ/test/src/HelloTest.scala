package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

object HelloTest extends TestSuite {
  def tests = Tests {
    val params = new BeliefQParams
    test("hello test") {
      SimConfig.compile { new SurfaceCodeDecoder(params, 3, 2) }.doSim { dut =>
        sleep(1)
        println(dut.edges2D)
      }
    }
  }
}
