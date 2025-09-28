package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

class RepetitionCodeGeometry(n: Int) {
  val var_labels = (0 until n).toSet
  val chk_labels = (0 until (n - 1)).toSet
  val edge1 = { for(i <- 0 until (n - 1)) yield (i, i) }.toSet
  val edge2 = { for(i <- 0 until (n - 1)) yield (i + 1, i) }.toSet
  val edge_labels = edge1 union edge2
}

/*
object SimRepetitionCode extends App {
  val params = new BeliefQParams()
  val geo = new RepetitionCodeGeometry(5)
  println(f"geo edges = ${geo.edge_labels}")
  // arbitrary fixed numbers just for testing
  val log_priors = List(8, 9, 55, 6, 7)
  val syndromes = List(true, false, true, true)
  SimConfig.compile {
    val dut = new BeliefQ(params, geo.var_labels, geo.chk_labels, geo.edge_labels)
    dut.controller.state.simPublic()
    dut.graph.converged.simPublic()
    for(v <- geo.var_labels) {
      dut.graph.variables(v).decision.simPublic()
      dut.graph.variables(v).prior.simPublic()
    }
    for(e <- geo.edge_labels) {
      dut.graph.edges(e).toC.simPublic()
      dut.graph.edges(e).toV.simPublic()
    }
    dut
  }.doSim { dut =>
    dut.inputs.valid #= false
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.assertReset()
    sleep(100)
    cd.deassertReset()
    sleep(100)
    for(v <- geo.var_labels) {
      dut.inputs.initial_priors(v) #= log_priors(v)
    }
    val is_ready = !(cd.waitSamplingWhere(5000) { dut.inputs.ready.toBoolean })
    assert(is_ready)
    dut.inputs.valid #= true
    for(c <- geo.chk_labels) {
      dut.inputs.syndromes(c) #= syndromes(c)
    }
    cd.waitSampling()
    dut.inputs.valid #= false

    for(t <- 0 until 100) {
      val state = dut.controller.state.toEnum
      if(state != State.result_valid) {
        println(f"t = $t, state = ${state}")
      }
      if(state == State.start_computing_cToV) {
        for(e <- geo.edge_labels) {
          println(f"v to c at ${e} = ${dut.graph.edges(e).toC.toBigDecimal}")
        }
      }
      if(state == State.start_decide) {
        for(e <- geo.edge_labels) {
          println(f"c to v at ${e} = ${dut.graph.edges(e).toV.toBigDecimal}")
        }
      }
      if(state == State.checking_decision) {
        for(v <- geo.var_labels) {
          println(f"decision(${v}) = ${dut.graph.variables(v).decision.toBoolean}")
        }
        println(f"overall converge check = ${dut.graph.converged.toBoolean}")
      }
      cd.waitSampling()
    }
    /*
    val converged = !(cd.waitSamplingWhere(5000) { dut.outputs.valid.toBoolean })
    assert(converged)
    */
  }
}

*/
