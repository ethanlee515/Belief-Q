package beliefq
package relay

import spinal.core._
import spinal.lib._
import beliefq.dmem.State

class Variable(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val fromC = in port Vec.fill(deg)(message_t())
  val toC = out port Vec.fill(deg)(Flow(message_t))
  val iter0 = in port Bool()
  val prior_in = in port message_t()
  val state = in port State()
  val decision = out port Reg(Bool())
  val bias_delays = 1
  /* -- logic -- */
  val llr = Reg(message_t())
  val prior = Reg(message_t())
  val biasL = Reg(message_t())
  val biasR = Reg(message_t())
  val bias = Reg(message_t())
  val gamma = Reg(gamma_t())
  val gamma_compl = Reg(gamma_t())
  when(state === State.loading_inputs) {
    prior := prior_in
    gamma := BigDecimal(0) //TODO
    gamma_compl := BigDecimal(1) //TODO
  }
  biasL := (gamma_compl * prior).truncated
  biasR := (gamma * llr).truncated
  when(state === State.computing_bias) {
    when(iter0) {
      bias := prior
    } otherwise {
      bias := (biasL + biasR).truncated
    }
  }
  val sumMessages = new SumOfMessages(params, deg + 1)
  val sumMessageDelays = sumMessages.delays
  for(i <- 0 until deg) {
    sumMessages.messages(i) := fromC(i)
  }
  sumMessages.messages(deg) := bias
  val start = (state === State.start_summing_messages)
  val valid = Delay(start, sumMessageDelays, init=False)
  when(valid) {
    llr := sumMessages.result
  }
  for(i <- 0 until deg) {
    val message = message_t()
    message := (sumMessages.result - fromC(i)).truncated
    toC(i).payload := RegNext(message)
    toC(i).valid := RegNext(valid)
  }
  decision := sumMessages.result.isNegative
}
