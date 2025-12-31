package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.relay._

/* Top level test
 * Check if Relay converges */
object TestRelay extends TestSuite {
  def tests = Tests {
    val num_tests = 500
    val var_labels = (0 until SimData.num_vars).toSet
    val chk_labels = (0 until SimData.num_checks).toSet
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
    test("Relay converges") {
      val params = new BeliefQParams()
      val converged = syndromes_batch.map { syndromes =>
        val vanillaBP = new reference.VanillaBP(var_labels, chk_labels, SimData.edges, syndromes, log_priors)
        vanillaBP.doBP(300) != None
      }
      SimConfig.compile { new Relay(params, var_labels, chk_labels, SimData.edges) }.doSim { dut =>
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
          val syndromes = syndromes_batch(i)
          val is_ready = !(cd.waitSamplingWhere(500) { dut.inputs.ready.toBoolean })
          assert(is_ready)
          dut.inputs.valid #= true
          for(c <- chk_labels) {
            dut.inputs.syndromes(c) #= syndromes(c)
          }
          cd.waitSampling()
          dut.inputs.valid #= false
          cd.waitSampling()
          cd.waitSampling()
          cd.waitSampling()
          val done = !(cd.waitSamplingWhere(30000) {
            dut.outputs.valid.toBoolean || dut.failed.toBoolean
          })
          assert(done)
          //println(f"relay test #${i}")
          // If Vanilla BP converges, maybe Relay should too.
          if(converged(i)) {
            assert(dut.outputs.valid.toBoolean)
          }
          // When convergence is claimed, output is well-formed
          if(dut.outputs.valid.toBoolean) {
            for(c <- chk_labels) {
              val geo = new TannerGraphGeometry(var_labels, chk_labels, SimData.edges)
              val vars = geo.get_neighboring_variables(c).toSeq
              val neighbor_syndromes = (vars.map {
                v => dut.outputs.corrections(v).toBoolean
              })
              val syndrome_out = neighbor_syndromes.reduce(_ ^ _)
              assert(syndrome_out == syndromes(c))
              //println(f"check label = ${c}, expected ${syndromes(c)}, seen ${syndrome_out}")
              //println(f"neighbor syndromes = ${neighbor_syndromes}")
            }
          }
        }
      }
    }
  }
}

