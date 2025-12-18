package beliefq
package test

import reference._
import utest._
import utest.assert
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import beliefq.relay._

object TestQualityEval extends TestSuite {
  import Sampler._

  def tests = Tests {
    val params = new BeliefQParams(8, 8)

    test("Quality Evaluation") {
      val var_labels = Set(11, 22, 33, 44, 55, 66)
      SimConfig.compile { new QualityEval(params, var_labels) }.doSim { dut =>
        dut.initial_priors(11) #= 1
        dut.initial_priors(22) #= 2
        dut.initial_priors(33) #= 3
        dut.initial_priors(44) #= 4
        dut.initial_priors(55) #= 5
        dut.initial_priors(66) #= 6
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        cd.waitSampling()
        dut.corrections_in_valid #= true
        dut.corrections_in(11) #= true
        dut.corrections_in(22) #= true
        dut.corrections_in(33) #= false
        dut.corrections_in(44) #= false
        dut.corrections_in(55) #= true
        dut.corrections_in(66) #= false
        cd.waitSampling()
        dut.corrections_in_valid #= false
        for(i <- 0 until 50) {
          cd.waitSampling()
        }
        assert(dut.best_decoding_quality.toBigDecimal == 8)
      }
    }
  }
}
