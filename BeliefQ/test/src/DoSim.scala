package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert

object DoSim extends App {
  val params = new BeliefQParams()
  val d = 3
  val num_meas = 1
  val logPriorSampler = new LogPriorSampler(d, num_meas)
  val log_prior = logPriorSampler.results
  val geo = logPriorSampler.geo
  val tanner_geo = new TannerGraphGeometry(geo.variables, geo.factors, geo.edges)
  val syndromeSampler = new SyndromeSampler(log_prior, tanner_geo)
  val syndromes = syndromeSampler.syndromes
  println(f"generated syndromes = ${syndromes}")
  println(f"generated priors = ${log_prior}")
  SimConfig.compile {
    val dut = new VanillaBP(params, geo.variables, geo.factors, geo.edges)
    dut.controller.state.simPublic()
    dut.graph.converged.simPublic()
    for(v <- geo.variables) {
      dut.graph.variables(v).decision.simPublic()
      dut.graph.variables(v).prior.simPublic()
    }
    for(e <- geo.edges) {
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
    dut.inputs.valid #= true
    for(v <- geo.variables) {
      dut.inputs.initial_priors(v) #= log_prior(v)
    }
    for(c <- geo.factors) {
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
      cd.waitSampling()
    }
  }
}
