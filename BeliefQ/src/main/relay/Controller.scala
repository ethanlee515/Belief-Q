// Just your usual (?) state machine stuff

package beliefq
package relay

import spinal.core._
import spinal.lib._

class Controller[V, C](params: BeliefQParams, graph: TannerGraph[V, C]) extends Component {
  import params._
  val start = in port Bool()
  val state = out port Reg(State()) init(State.idle)
  state.addAttribute("MAX_FANOUT", 16)
  val converged = in port Bool()
  // For delays
  val counter = Reg(UInt(8 bits)) init(0)
  val num_iters = Reg(UInt(8 bits)) init(0)
  val num_sols = out port Reg(UInt(8 bits)) init(0)
  val num_legs = Reg(UInt(8 bits)) init(0)
  // hmmmm lots of boilerplate
  switch(state) {
    is(State.idle) {
      when(start) {
        counter := 0
        num_iters := 0
        num_sols := 0
        num_legs := 0
        state := State.loading_inputs
      }
    }
    is(State.loading_inputs) {
      // TODO optimize this "extra cycle" away
      state := State.rerandomize_weights
    }
    is(State.start_computing_bias) {
      state := State.computing_bias
      counter := 1
    }
    is(State.computing_bias) {
      when(counter === graph.bias_delays) {
        state := State.start_summing_messages
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.start_summing_messages) {
      state := State.summing_messages
      counter := 1
    }
    is(State.summing_messages) {
      when(counter === graph.sumMessageDelays) {
        state := State.variables_decide
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.variables_decide) {
      state := State.checking_decision
    }
    is(State.checks_decide) {
      state := State.checking_decision
    }
    is(State.checking_decision) {
      when(converged) {
        num_sols := num_sols + 1
        state := State.result_valid
      } elsewhen(num_iters < max_iters) {
        state := State.start_computing_cToV
      } elsewhen(num_legs === max_legs) {
        state := State.failed
      } otherwise {
        state := State.rerandomize_weights
      }
    }
    is(State.start_computing_cToV) {
      state := State.computing_cToV
      counter := 1
    }
    is(State.computing_cToV) {
      when(counter === graph.cToVDelays) {
        state := State.start_computing_bias
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.result_valid) {
      when(num_legs === max_legs || num_sols === max_sols) {
        state := State.idle
      } otherwise {
        state := State.rerandomize_weights
      }
    }
    is(State.rerandomize_weights) {
      state := State.start_computing_cToV
      num_legs := num_legs + 1
      num_iters := 0
    }
    is(State.failed) {
      state := State.idle
    }
  }
}
