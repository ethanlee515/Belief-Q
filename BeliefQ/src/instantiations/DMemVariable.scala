package beliefq
package relay

import spinal.core._
import spinal.lib._

class DMemVariable(
    params: BeliefQParams,
    deg: Int,
    gamma_in: BigDecimal) extends Variable {
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
  when(state === State.loading_inputs) {
    prior := prior_in
    gamma := gamma_in
    gamma_compl := 1 - gamma_in
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
