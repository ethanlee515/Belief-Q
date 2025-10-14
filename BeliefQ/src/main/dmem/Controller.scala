// Just your usual (?) state machine stuff

package beliefq
package dmem

import spinal.core._
import spinal.lib._

class Controller[V, C](graph: TannerGraph[V, C]) extends Component {
  val start = in port Bool()
  val state = out port Reg(State()) init(State.idle)
  state.addAttribute("MAX_FANOUT", 16)
  val converged = in port Bool()
  // TODO Don't actually need 8 bits here
  // take max over all delays...
  val counter = Reg(UInt(8 bits)) init(0)
  // hmmmm lots of boilerplate
  // also missing a few more still, with the DMem stuff and whatnot
  switch(state) {
    is(State.idle) {
      when(start) {
        state := State.loading_inputs
      }
    }
    is(State.loading_inputs) {
      state := State.start_computing_cToV
    }
    is(State.start_computing_bias) {
      // TODO is this actually single-cycle?
      state := State.start_summing_messages
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
        // Result is either valid, or need to restart
        state := State.result_valid
      } otherwise {
        state := State.start_computing_cToV
      }
    }
    is(State.start_computing_cToV) {
      state := State.computing_cToV
      counter := 1
    }
    is(State.computing_cToV) {
      when(counter === graph.cToVDelays) {
        state := State.start_computing_bias
        // TODO update prior and stuff?
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.result_valid) {
      state := State.idle
    }
  }
}
