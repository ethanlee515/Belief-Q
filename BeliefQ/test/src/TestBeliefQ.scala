package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

/*

object TestBeliefQ extends TestSuite {
  def tests = Tests {
    val params = new BeliefQParams()
    test("eventual convergence") {
      val d = 5
      val num_meas = 1
      val logPriorSampler = new LogPriorSampler(d, num_meas)
      val log_prior = logPriorSampler.results
      val geo = logPriorSampler.geo
      val tanner_geo = new TannerGraphGeometry(params, geo.variables, geo.factors, geo.edges)
      val syndromeSampler = new SyndromeSampler(log_prior, tanner_geo)
      val syndromes = syndromeSampler.syndromes
      println(f"generated syndromes = ${syndromes}")
      SimConfig.compile { new BeliefQ(params, geo.variables, geo.factors, geo.edges) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        dut.inputs.valid #= true
        for(v <- geo.variables) {
          dut.inputs.initial_priors(v) #= log_prior(v)
        }
        for(c <- geo.factors) {
          dut.inputs.syndromes(c) #= syndromes(c)
        }
        cd.waitSampling()
        dut.inputs.valid #= false
        val converged = !(cd.waitSamplingWhere(5000) { dut.outputs.valid.toBoolean })
        assert(converged)
        for(v <- geo.variables) {
          if(dut.outputs.corrections(v).toBoolean)
          println(f"need to correct: ${v}")
        }
      }
    }
  }
}
*/
