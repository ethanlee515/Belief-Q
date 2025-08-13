package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

object HelloTest extends TestSuite {
  def tests = Tests {
    test("hello test") {
      SimConfig.compile { new SurfaceCodeDecoder(15) }.doSim { dut =>
        sleep(1)
        assert(dut.v.toInt == 15)
      }
    }
  }
}