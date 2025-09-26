package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

object TestBeliefQ extends TestSuite {
  def tests = Tests {
    val params = new BeliefQParams()
    test("vanilla BP matches reference") {
      val geo = SimData.geo
      SimConfig.compile { new BeliefQ(params, geo.var_labels, geo.chk_labels, geo.edge_labels) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        // why not
        for(v <- geo.var_labels) {
          dut.inputs.initial_priors(v) #= SimData.log_priors(v)
        }
        for(i <- 2 until 1000) {
          if(SimData.is_converged(i)) {
            val syndromes = SimData.syndromes_batch(i)
            println(f"doing iteration i = ${i} with syndromes ${syndromes}")
            println(f"expected result = ${SimData.ehat_bp(i)}")
            val is_ready = !(cd.waitSamplingWhere(5000) { dut.inputs.ready.toBoolean })
            assert(is_ready)
            dut.inputs.valid #= true
            for(c <- geo.chk_labels) {
              dut.inputs.syndromes(c) #= syndromes(c)
            }
            cd.waitSampling()
            dut.inputs.valid #= false
            val converged = !(cd.waitSamplingWhere(5000) { dut.outputs.valid.toBoolean })
            assert(converged)
            /*
        for(v <- geo.variables) {
          if(dut.outputs.corrections(v).toBoolean)
          println(f"need to correct: ${v}")
        }
      val syndromes = syndromeSampler.syndromes
        */
          }
        }
      }
    }
  }
}

/*
  var converge_count = 0
  var diverge_count = 0
  for(i <- 0 until 1000) {
      converge_count += 1
    } else {
      diverge_count += 1
    }
    */
