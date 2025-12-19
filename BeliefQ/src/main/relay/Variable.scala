package beliefq
package relay

import scala.util.Random
import spinal.core._
import spinal.lib._

class Variable(params: BeliefQParams, deg: Int, seed: BigInt) extends Component {
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
  val rng = Lfsr64(params, seed)
  val five = message_t()
  five := BigDecimal("5")
  val rng_normed = Reg(message_t())
  rng_normed := ((rng.rng_norm1 * five) >> 3).truncated
  val one_eighth = message_t()
  one_eighth := BigDecimal("0.125")
  val seven_eighth = message_t()
  seven_eighth := BigDecimal("0.875")
  val rnd_gamma = (rng_normed - one_eighth).truncated
  val rnd_gamma_compl = (seven_eighth - rng_normed).truncated
  when(state === State.rerandomize_weights) {
    gamma := rnd_gamma
    gamma_compl := rnd_gamma_compl
  }
  when(state === State.loading_inputs) {
    prior := prior_in
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
