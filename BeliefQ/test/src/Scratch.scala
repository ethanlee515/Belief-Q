package beliefq
package test

import beliefq.generate._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

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
      val logPrior = new LogPriorSampler(3, 2)
      val syndromeSampler = new SyndromeSampler(logPrior.log_priors)
    }
  }
}
