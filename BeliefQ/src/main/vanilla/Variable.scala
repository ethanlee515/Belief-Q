package beliefq
package vanilla

import spinal.core._
import spinal.lib._

class Variable(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val fromC = in port Vec.fill(deg)(message_t())
  val toC = out port Vec.fill(deg)(Flow(message_t))
  val prior_in = in port message_t()
  val state = in port State()
  val decision = out port Reg(Bool())
  /* -- logic -- */
  val prior = Reg(message_t())
  when(state === State.loading_inputs) {
    prior := prior_in
  }
  val sumMessages = new SumOfMessages(params, deg + 1)
  val sumMessageDelays = sumMessages.delays
  for(i <- 0 until deg) {
    sumMessages.messages(i) := fromC(i)
  }
  sumMessages.messages(deg) := prior
  val start = (state === State.start_summing_messages)
  val valid = Delay(start, sumMessageDelays + 1, init=False)
  for(i <- 0 until deg) {
    val message = message_t()
    message := (sumMessages.result - fromC(i)).truncated
    toC(i).payload := RegNext(message)
    toC(i).valid := valid
  }
  decision := sumMessages.result.isNegative
}
