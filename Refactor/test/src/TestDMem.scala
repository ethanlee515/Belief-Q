package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.relay._

object TestDMem extends TestSuite {
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

    test("DMemBP converges") {
      val params = new RelayParams()
      val converged = syndromes_batch.map { syndromes =>
        val bp = new reference.DMemBP(
          var_labels,
          chk_labels,
          SimData.edges,
          syndromes,
          SimData.gammas,
          log_priors)
        bp.doBP(100) != None
      }
      var diverge_count = 0
      SimConfig.compile {
        new DMemBP(params, var_labels, chk_labels, SimData.edges, SimData.gammas)
      }.doSim { dut =>
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
          val done = !(cd.waitSamplingWhere(30000) {
            dut.outputs.valid.toBoolean || dut.failed.toBoolean
          })
          assert(done)
          if(converged(i) && !(dut.outputs.valid.toBoolean)) {
            diverge_count = diverge_count + 1
          }
          if(dut.outputs.valid.toBoolean) {
            val geo = new TannerGraphGeometry(var_labels, chk_labels, SimData.edges)
            for(c <- chk_labels) {
              val vars = geo.get_neighboring_variables(c).toSeq
              val syndrome_out = vars.map { v =>
                dut.outputs.corrections(v).toBoolean
              }.reduce(_ ^ _)
              assert(syndrome_out == syndromes(c))
            }
          }
        }
      }
      assert(diverge_count.toFloat / num_tests < 0.1)
    }
  }
}
