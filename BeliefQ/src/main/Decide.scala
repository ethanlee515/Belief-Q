// Convert a variable's state into a hard boolean outcome

package beliefq

import spinal.core._
import spinal.lib._

class Decide(params: BeliefQParams, num_messages: Int) extends Component {
  import params._
  /* -- IO -- */
  val messages = in port Vec.fill(num_messages)(message_t())
  val decision = out port Reg(Bool())
  val sumMessages = new SumOfMessages(params, num_messages)
  sumMessages.messages := messages
  decision := (sumMessages.result.isNegative)
  val delays = sumMessages.delays + 1
}

