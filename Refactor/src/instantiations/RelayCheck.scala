package beliefq
package relay

import spinal.core._
import spinal.lib._

class RelayCheck(
  params: BeliefQParams,
  deg: Int) extends Check {
  val relayparams : RelayParams = params.asInstanceOf[RelayParams]
  import relayparams._
  override val state = in port State()
  override val fromV = in port Vec.fill(deg)(Bits(var_msg_len bits))
  override val in_syndrome = in port Bool()
  val syndrome = Reg(Bool())
  override val toV = out port Vec.fill(deg)(Flow(Bits(chk_msg_len bits)))
  val cToV = new CToV(relayparams, deg)
  cToV.inputs.valid := (state === State.start_computing_cToV)
  cToV.inputs.payload.syndrome := syndrome
  cToV.inputs.raw_messages := fromV
  override val cToVDelays = cToV.delays
  override val neighbor_decisions = in port Vec.fill(deg)(Bool())
  override val satisfied = out port Reg(Bool())
  satisfied := (neighbor_decisions.xorR === syndrome)
  when(state === State.loading_inputs) {
    syndrome := in_syndrome
  }
  for(i <- 0 until deg) {
    toV(i).valid := cToV.output.valid
    toV(i).payload := cToV.output.payload(i).asBits
  }
}
