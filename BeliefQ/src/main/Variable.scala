package beliefq

import spinal.core._
import spinal.lib._

class Variable(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val fromC = in port Vec.fill(deg)(message_t())
  val toC = out port Vec.fill(deg)(Flow(message_t))
  val prior_in = in port message_t()
  val state = in port State()
  val decision = out port Bool()
  val prior = Reg(message_t())
  val vToC = new VToC(params, deg)
  vToC.inputs.valid := (state === State.start_computing_vToC)
  vToC.inputs.payload.prior := prior
  vToC.inputs.messages := fromC
  val vToCDelays = vToC.delays
  val decide = new Decide(params, deg + 1)
  val decisionDelays = decide.delays
  for(i <- 0 until deg) {
    decide.messages(i) := fromC(i)
  }
  decide.messages(deg) := prior
  val decideDelays = decide.delays
  decision := decide.decision
  when(state === State.loading_inputs) {
    prior := prior_in
  }
  for(i <- 0 until deg) {
    toC(i).payload := vToC.output.payload(i)
    toC(i).valid := vToC.output.valid
  }
}
