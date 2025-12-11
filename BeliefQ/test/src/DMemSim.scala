package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.dmem._
import beliefq.dmem.DMemBP

object DMemSim extends App {
  val var_labels = (0 until SimData.num_vars).toSet
  val chk_labels = (0 until SimData.num_checks).toSet
  val log_priors : Map[Int, BigDecimal] = {
    for(j <- 0 until SimData.num_vars) yield {
      j -> SimData.log_priors(j)
    }
  }.toMap
  val syndromes_batch = {
    for(i <- 0 until 5) yield {
      for(j <- 0 until SimData.num_checks) yield {
        j -> SimData.syndromes_batch(i)(j)
      }
    }.toMap
  }
  val params = new BeliefQParams()
  val syndromes = syndromes_batch(0)
  SimConfig.compile {
    val dut = new DMemBP(params, var_labels, chk_labels, SimData.edges, SimData.gammas)
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
    dut.inputs.valid #= false
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.assertReset()
    sleep(100)
    cd.deassertReset()
    sleep(100)
    dut.inputs.valid #= true
    for(v <- var_labels) {
      dut.inputs.initial_priors(v) #= log_priors(v)
    }
    for(c <- chk_labels) {
      dut.inputs.syndromes(c) #= syndromes(c)
    }
    println(f"setting input when state = ${dut.controller.state.toEnum}")
    cd.waitSampling()
    dut.inputs.valid #= false
    for(t <- 0 until 100) {
      val state = dut.controller.state.toEnum
      if(state != State.result_valid) {
        println(f"t = $t, state = ${state}")
      }
      /*
      if(state == State.start_computing_cToV) {
        for(e <- geo.edges) {
          println(f"v to c at ${e} = ${dut.graph.edges(e).toC.toBigDecimal}")
        }
      }
      if(state == State.start_decide) {
        for(e <- geo.edges) {
          println(f"c to v at ${e} = ${dut.graph.edges(e).toV.toBigDecimal}")
        }
      }
      if(state == State.checking_decision) {
        for(v <- geo.variables) {
          println(f"decision(${v}) = ${dut.graph.variables(v).decision.toBoolean}")
        }
        println(f"overall converge check = ${dut.graph.converged.toBoolean}")
      }
      */
      cd.waitSampling()
    }
  }
}


