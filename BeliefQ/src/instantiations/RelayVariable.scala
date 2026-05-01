package beliefq
package relay

import scala.util.Random
import spinal.core._
import spinal.lib._

class RelayVariable(
    params: BeliefQParams,
    deg: Int, seed: BigInt) extends Variable {
  val relayparams : RelayParams = params.asInstanceOf[RelayParams]
  import relayparams._
  /* -- IO -- */
  override val fromC_raw = in port Vec.fill(deg)(Bits(chk_msg_len bits))
  override val toC_raw = out port Vec.fill(deg)(Flow(Bits(var_msg_len bits)))
  override val iter0 = in port Bool()
  override val prior_in = in port message_t()
  override val state = in port State()
  override val decision = out port Reg(Bool())
  /* -- logic -- */
  val biasDelays = 1
  val fromC = Vec.fill(deg)(message_t())
  val toC = Vec.fill(deg)(Flow(message_t))
  for(i <- 0 until deg) {
    fromC(i).assignFromBits(fromC_raw(i))
    toC_raw(i).payload := toC(i).payload.asBits 
    toC_raw(i).valid := toC(i).valid
  }
  val llr = Reg(message_t())
  val prior = Reg(message_t())
  val biasL = Reg(message_t())
  val biasR = Reg(message_t())
  val bias = Reg(message_t())
  val gamma = Reg(gamma_t())
  val gamma_compl = Reg(gamma_t())
  val rng = Lfsr64(relayparams, seed)
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
  when(state === State.start_computing_vToC) {
    when(iter0) {
      bias := prior
    } otherwise {
      bias := (biasL + biasR).truncated
    }
  }
  val sumMessages = new SumTree(message_t, deg + 1)
  override val vToCDelays = biasDelays + sumMessages.delays
  for(i <- 0 until deg) {
    sumMessages.inputs(i) := fromC(i)
  }
  sumMessages.inputs(deg) := bias
  val start = Delay(state === State.start_computing_vToC, biasDelays, init=False)
  val valid = Delay(start, sumMessages.delays, init=False)
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
