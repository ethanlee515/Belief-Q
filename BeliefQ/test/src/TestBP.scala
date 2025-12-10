package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.vanilla._
import beliefq.dmem._

object TestBeliefQ extends TestSuite {
  def tests = Tests {
    val num_tests = 1000
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
    test("vanilla BP matches reference") {
      val params = new BeliefQParams()
      val correct_results = syndromes_batch.map { syndromes =>
        val vanillaBP = new reference.VanillaBP(var_labels, chk_labels, SimData.edges, syndromes, log_priors)
        vanillaBP.doBP(300)
      }
      SimConfig.compile { new VanillaBP(params, var_labels, chk_labels, SimData.edges) }.doSim { dut =>
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
              val is_ready = !(cd.waitSamplingWhere(500) { dut.inputs.ready.toBoolean })
              assert(is_ready)
              dut.inputs.valid #= true
              for(c <- chk_labels) {
                dut.inputs.syndromes(c) #= syndromes(c)
              }
              cd.waitSampling()
              dut.inputs.valid #= false
              val converged = !(cd.waitSamplingWhere(2000) { dut.outputs.valid.toBoolean })
              assert(converged)
              for(v <- var_labels) {
                assert(dut.outputs.corrections(v).toBoolean == res(v))
              }
            }
            case None => { }
          }
        }
      }
    }

    test("DMemBP matches reference") {
      val correct_results = syndromes_batch.map { syndromes =>
        val bp = new reference.DMemBP(var_labels, chk_labels, SimData.edges, syndromes, SimData.gammas, log_priors)
        bp.doBP(100)
      }
      val params = new BeliefQParams(16, 16, 16)
      SimConfig.compile { new DMemBP(params, var_labels, chk_labels, SimData.edges, SimData.gammas) }.doSim { dut =>
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
              val is_ready = !(cd.waitSamplingWhere(500) { dut.inputs.ready.toBoolean })
              assert(is_ready)
              dut.inputs.valid #= true
              for(c <- chk_labels) {
                dut.inputs.syndromes(c) #= syndromes(c)
              }
              cd.waitSampling()
              dut.inputs.valid #= false
              val converged = !(cd.waitSamplingWhere(5000) { dut.outputs.valid.toBoolean })
              assert(converged)
              for(v <- var_labels) {
                assert(dut.outputs.corrections(v).toBoolean == res(v))
              }
              println(f"DMemBP test #${i} passed")
            }
            case None => {
              println(f"DMemBP test #${i} skipped; reference impl did not converge")
            }
          }
        }
      }
    }

/*
    test("vanilla BP step-by-step test") {
      val syndromes = syndromes_batch(0)
      val ref = new reference.VanillaBP(var_labels, chk_labels, SimData.edges, syndromes, log_priors)
      SimConfig.compile {
        val dut = new VanillaBP(params, var_labels, chk_labels, SimData.edges)
        dut.controller.state.simPublic()
        dut.graph.converged.simPublic()
        for(v <- var_labels) {
          dut.graph.variables(v).decision.simPublic()
          dut.graph.variables(v).prior.simPublic()
        }
        for(e <- SimData.edges) {
          dut.graph.edges(e).toC.simPublic()
          dut.graph.edges(e).toV.simPublic()
        }
        dut.delayed_inputs_valid.simPublic()
        dut
      }.doSim { dut =>
        for(v <- var_labels) {
          dut.inputs.initial_priors(v) #= log_priors(v)
        }
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        assert(dut.inputs.ready.toBoolean)
        dut.inputs.valid #= true
        for(c <- chk_labels) {
          dut.inputs.syndromes(c) #= syndromes(c)
        }
        cd.waitSampling()
        dut.inputs.valid #= false
        // synchronize dut and software reference
        val sync = !(cd.waitSamplingWhere(2000) { dut.controller.state.toEnum == State.start_computing_cToV })
        assert(sync)
        for(e <- SimData.edges) {
          val m = dut.graph.edges(e).toC.toBigDecimal
          assert(m == ref.vToCs(e))
        }
        // simulate until difference found
        var fuel = 500
        var done = false
        while(!done && fuel > 0) {
          cd.waitSampling()
          fuel -= 1
          val state = dut.controller.state.toEnum
          println(f"t = ${500 - fuel}, state = $state")
          state match {
            case State.start_computing_cToV => {
              ref.step()
              val refstate = ref.state
              assert(refstate == reference.State.computeCToV)
              for(e <- SimData.edges) {
                val m = dut.graph.edges(e).toC.toBigDecimal
                assert(m == ref.vToCs(e))
              }
            }
            case State.start_computing_vToC => {
              ref.step()
              val refstate = ref.state
              assert(refstate == reference.State.computeVToC)
            }
            case State.start_decide => {
              ref.step()
              val refstate = ref.state
              assert(refstate == reference.State.computeDecisions)
              for(e <- SimData.edges) {
                val m = dut.graph.edges(e).toV.toBigDecimal
                assert(m == ref.cToVs(e))
              }
            }
            case State.checking_decision => {
              ref.step()
              val refstate = ref.state
              assert(refstate == reference.State.checkConvergence)
            }
            case State.result_valid => {
              ref.step()
              assert(ref.isDone())
              done = true
              // TODO assert results match?
            }
            case default => {}
          }
        }
        assert(done)
      }
    }
    */
  }
}
