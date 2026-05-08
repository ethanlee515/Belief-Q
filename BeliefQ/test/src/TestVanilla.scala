package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.relay._

object TestVanilla extends TestSuite {
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

    test("VanillaBP matches reference") {
      val correct_results = syndromes_batch.map { syndromes =>
        val vanillaBP = new reference.VanillaBP(
          var_labels,
          chk_labels,
          SimData.edges,
          syndromes,
          log_priors)
        vanillaBP.doBP(300)
      }
      val params = new RelayParams(pipeline_converged=true) {
        override val max_sols = 1
      }
      SimConfig.compile {
        new VanillaBP(params, var_labels, chk_labels, SimData.edges)
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
          val correct_result = correct_results(i)
          correct_result match {
            case Some(res) => {
              val is_ready = !(cd.waitSamplingWhere(500) {
                dut.inputs.ready.toBoolean
              })
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
              assert(dut.outputs.valid.toBoolean)
              for(v <- var_labels) {
                assert(dut.outputs.corrections(v).toBoolean == res(v))
              }
            }
            case None => { }
          }
        }
      }
    }
  }
}
