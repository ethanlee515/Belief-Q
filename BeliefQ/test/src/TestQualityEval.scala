package beliefq
package test

import spinal.core._
import spinal.core.sim._
import utest._
import utest.assert
import beliefq.relay._

object TestQualityEval extends TestSuite {
  def tests = Tests {
    test("Quality Evaluation") {
      val params = new BeliefQParams()
      val var_labels = Set(11, 22, 33, 44, 55, 66)
      SimConfig.compile { new QualityEval(params, var_labels) }.doSim { dut =>
        val cd = dut.clockDomain
        dut.rst #= false
        dut.corrections_in_valid #= false
        for(v <- var_labels) {
          dut.corrections_in(v) #= false
        }
        dut.initial_priors(11) #= 1
        dut.initial_priors(22) #= 2
        dut.initial_priors(33) #= 3
        dut.initial_priors(44) #= 4
        dut.initial_priors(55) #= 5
        dut.initial_priors(66) #= 6

        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)

        dut.rst #= true
        cd.waitSampling()
        dut.rst #= false

        dut.corrections_in_valid #= true
        dut.corrections_in(11) #= true
        dut.corrections_in(22) #= true
        dut.corrections_in(33) #= false
        dut.corrections_in(44) #= false
        dut.corrections_in(55) #= true
        dut.corrections_in(66) #= false
        cd.waitSampling()
        dut.corrections_in_valid #= false
        dut.corrections_in(11) #= false

        val first_done = !(cd.waitSamplingWhere(30) {
          dut.corrections_out_valid.toBoolean
        })
        assert(first_done)
        assert(dut.best_decoding_quality.toBigDecimal == 8)
        assert(dut.corrections_out(11).toBoolean)
        assert(dut.corrections_out(22).toBoolean)
        assert(!dut.corrections_out(33).toBoolean)
        assert(!dut.corrections_out(44).toBoolean)
        assert(dut.corrections_out(55).toBoolean)
        assert(!dut.corrections_out(66).toBoolean)

        cd.waitSampling(3)
        dut.corrections_in_valid #= true
        dut.corrections_in(11) #= true
        dut.corrections_in(22) #= true
        dut.corrections_in(33) #= false
        dut.corrections_in(44) #= false
        dut.corrections_in(55) #= false
        dut.corrections_in(66) #= false
        cd.waitSampling()
        dut.corrections_in_valid #= false
        dut.corrections_in(11) #= false

        val second_done = !(cd.waitSamplingWhere(30) {
          dut.corrections_out_valid.toBoolean
        })
        assert(second_done)
        assert(dut.best_decoding_quality.toBigDecimal == 3)
        assert(dut.corrections_out(11).toBoolean)
        assert(dut.corrections_out(22).toBoolean)
        assert(!dut.corrections_out(33).toBoolean)
        assert(!dut.corrections_out(44).toBoolean)
        assert(!dut.corrections_out(55).toBoolean)
        assert(!dut.corrections_out(66).toBoolean)
      }
    }
  }
}
