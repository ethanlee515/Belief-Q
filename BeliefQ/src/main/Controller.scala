// Just your usual (?) state machine stuff

package beliefq

import spinal.core._
import spinal.lib._

class Controller[V, C](graph: TannerGraph[V, C]) extends Component {
  val start = in port Bool()
  val state = out port Reg(State()) init(State.idle)
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
      state := State.start_computing_vToC
    }
    is(State.start_computing_vToC) {
      state := State.computing_vToC
      counter := 1
    }
    is(State.computing_vToC) {
      when(counter === graph.vToCDelays) {
        state := State.start_computing_cToV
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.start_computing_cToV) {
      state := State.computing_cToV
      counter := 1
    }
    is(State.computing_cToV) {
      when(counter === graph.cToVDelays) {
        state := State.start_decide
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.start_decide) {
      state := State.deciding
      counter := 1
    }
    is(State.deciding) {
      when(counter === graph.decisionDelays) {
        state := State.checking_decision
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.checking_decision) {
      // TODO this should be a single-cycle thing
      when(converged) {
        // Result is either valid, or need to restart
        state := State.result_valid
      } otherwise {
        // TODO update prior and stuff?
        state := State.start_computing_vToC
      }
    }
    is(State.result_valid) {
      state := State.idle
    }
  }
}
