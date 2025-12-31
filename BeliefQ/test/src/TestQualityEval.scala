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
      println("Quality evaluation test...")
      val var_labels = Set(11, 22, 33, 44, 55, 66)
      SimConfig.compile { new QualityEval(params, var_labels) }.doSim { dut =>
        dut.corrections_in_valid #= false
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
        dut.corrections_in(11) #= false
//        println("Quality evaluation test: waiting for first convergence...")
        val converged = !(cd.waitSamplingWhere(30) { dut.corrections_out_valid.toBoolean })
        assert(converged)
//        println("Quality evaluation test: first convergence...")
        val res1 = dut.best_decoding_quality.toBigDecimal
        assert(res1 == 8)
        assert(dut.corrections_out(11).toBoolean)
        assert(dut.corrections_out(22).toBoolean)
        assert(!dut.corrections_out(33).toBoolean)
        assert(!dut.corrections_out(44).toBoolean)
        assert(dut.corrections_out(55).toBoolean)
        assert(!dut.corrections_out(66).toBoolean)
        cd.waitSampling()
        cd.waitSampling()
        cd.waitSampling()
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
//        println("Quality evaluation test: waiting for second convergence...")
        val converged2 = !(cd.waitSamplingWhere(30) { dut.corrections_out_valid.toBoolean })
        assert(converged2)
//        println("Quality evaluation test: second convergence.")
        val res2 = dut.best_decoding_quality.toBigDecimal
        assert(res2 == 3)
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
