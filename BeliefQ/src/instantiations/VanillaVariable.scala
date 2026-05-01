package beliefq
package relay

import spinal.core._
import spinal.lib._

class VanillaVariable(
    params: BeliefQParams,
    deg: Int) extends Variable {
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
  val fromC = Vec.fill(deg)(message_t())
  val toC = Vec.fill(deg)(Flow(message_t))
  for(i <- 0 until deg) {
    fromC(i).assignFromBits(fromC_raw(i))
    toC_raw(i).payload := toC(i).payload.asBits
    toC_raw(i).valid := toC(i).valid
  }
  val prior = Reg(message_t())
  when(state === State.loading_inputs) {
    prior := prior_in
  }
  val sumMessages = new SumTree(message_t, deg + 1)
  override val vToCDelays = sumMessages.delays + 1
  for(i <- 0 until deg) {
    sumMessages.inputs(i) := fromC(i)
  }
  sumMessages.inputs(deg) := prior
  val valid = Delay(state === State.start_computing_vToC, sumMessages.delays, init=False)
  for(i <- 0 until deg) {
    val message = message_t()
    message := (sumMessages.result - fromC(i)).truncated
    toC(i).payload := RegNext(message)
    toC(i).valid := RegNext(valid)
  }
  decision := sumMessages.result.isNegative
}
