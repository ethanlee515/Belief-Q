// Just your usual (?) state machine stuff

package beliefq

import spinal.core._
import spinal.lib._

class Controller extends Component {
  val start = in port Bool()
  val state = out port Reg(State()) init(State.idle)
  // TODO delay math...
  val vToCDelays = 15
  val cToVDelays = 15
  val decisionDelays = 15
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
      // TODO init priors
      // TODO init edges
      // TODO init syndromes
      // None of which belong here I suppose.
      state := State.start_computing_vToC
    }
    is(State.start_computing_vToC) {
      state := State.computing_vToC
      // TODO 0 or 1? off-by-1 errors man...
      counter := 0
    }
    is(State.computing_vToC) {
      // TODO Some fixed delay...
      when(counter === 15) {
        state := State.start_computing_cToV
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.start_computing_cToV) {
      state := State.computing_cToV
    }
    is(State.computing_cToV) {
      when(counter === 15) {
        state := State.start_computing_decision
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.start_computing_decision) {
      state := State.computing_decision
    }
    is(State.computing_decision) {
      when(counter === 15) {
        state := State.start_checking_decision
      } otherwise {
        counter := counter + 1
      }
    }
    is(State.start_checking_decision) {
      state := State.checking_decision
    }
    is(State.checking_decision) {
      when(counter === 15) {
        // TODO now this is the fun part
        // Result is either valid, or need to restart
        state := State.result_valid
      } otherwise {
        counter := counter + 1
      }
    }
  }
}
