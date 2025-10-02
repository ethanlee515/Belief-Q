package beliefq

import spinal.core._
import spinal.lib._

case class VToCInputs(params: BeliefQParams, deg: Int) extends Bundle {
  import params._
  val messages = Vec.fill(deg)(message_t())
  val prior = message_t()
}

class VToC(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val inputs = in port Flow(VToCInputs(params, deg))
  val output = out port Flow(Vec.fill(deg)(Reg(message_t())))
  /* logic */
  val sumMessages = new SumOfMessages(params, deg + 1)
  for(i <- 0 until deg) {
    sumMessages.messages(i) := inputs.messages(i)
  }
  sumMessages.messages(deg) := inputs.prior
  for(i <- 0 until deg) {
    output.payload(i) := (sumMessages.result - inputs.messages(i)).truncated
  }
  val delays = sumMessages.delays + 1
  output.valid := Delay(inputs.valid, delays, init=False)
}

