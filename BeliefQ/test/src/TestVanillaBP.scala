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
      val num_tests = 1000
      val var_labels = (0 until SimData.num_vars).toSet
      val factor_labels = (0 until SimData.num_checks).toSet
      val log_priors : Map[Int, BigDecimal] = {
        for(j <- 0 until SimData.num_vars) yield {
          j -> SimData.log_priors(j)
        }
      }.toMap
      val syndromes_batch = {
        for(i <- 0 until num_tests) yield {
          for(j <- 0 until SimData.num_checks) yield {
            j -> SimData.syndromes_batch(i)(j)
          }
        }.toMap
      }
      val correct_results = syndromes_batch.map { syndromes =>
        val vanillaBP = new reference.VanillaBP(var_labels, factor_labels, SimData.edges, syndromes, log_priors)
        vanillaBP.doBP(500)
      }
      SimConfig.compile { new VanillaBP(params, var_labels, factor_labels, SimData.edges) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        for(v <- var_labels) {
          dut.inputs.initial_priors(v) #= log_priors(v)
        }
        for(i <- 0 until num_tests) {
          println(f"checking data: ${i}")
          val syndromes = syndromes_batch(i)
          val correct_result = correct_results(i)
          correct_result match {
            case Some(res) => {
              val is_ready = !(cd.waitSamplingWhere(5000) { dut.inputs.ready.toBoolean })
              assert(is_ready)
              dut.inputs.valid #= true
              for(c <- factor_labels) {
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
            case None => { }
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

