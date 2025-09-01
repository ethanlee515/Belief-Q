package beliefq
package test

import beliefq.generate._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

object TestBeliefQ extends TestSuite {
  def tests = Tests {
    val params = new BeliefQParams()
    test("hello test") {
      val d = 3
      val num_meas = 2
      val logPriorSampler = new LogPriorSampler(d, num_meas)
      val log_prior = logPriorSampler.results
      val geo = logPriorSampler.geo
      val tanner_geo = new TannerGraphGeometry(params, geo.variables, geo.factors, geo.edges)
      val syndromeSampler = new SyndromeSampler(log_prior, tanner_geo)
      val syndromes = syndromeSampler.syndromes
      println(f"generated syndromes = ${syndromes}")
      SimConfig.compile { new FlattenedBeliefQ(params, geo.variables, geo.factors, geo.edges) }.doSim { dut =>
        dut.input_valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        dut.input_valid #= true
        for(v <- geo.variables) {
          dut.initial_priors(v) #= log_prior(v)
        }
        for(c <- geo.factors) {
          dut.syndromes(c) #= syndromes(c)
        }
        cd.waitSampling()
        dut.input_valid #= false
        assert(!cd.waitSamplingWhere(1000) { dut.output_valid.toBoolean })
        for(v <- geo.variables) {
          println(f"${v}: ${dut.corrections(v)}")
        }
      }
    }
  }
}
