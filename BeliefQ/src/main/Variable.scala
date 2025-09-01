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
  // TODO
  val vToCDelays = 10
  val decisionDelays = 10
  val prior = Reg(message_t())
  val decide = new Decide(params, deg)
  decide.messages := fromC
  val decideDelays = decide.delays
  decision := decide.decision
  when(state === State.loading_inputs) {
    prior := prior_in
  }
  for(f <- toC) {
    f.setIdle()
  }
}
