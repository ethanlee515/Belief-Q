package beliefq
package relay

import spinal.core._
import spinal.lib._

class Check(
  params: BeliefQParams,
  deg: Int) extends Component {
  val relayparams : RelayParams = params.asInstanceOf[RelayParams]
  import relayparams._
  val state = in port State()
  val fromV = in port Vec.fill(deg)(Bits(var_msg_len bits))
  val in_syndrome = in port Bool()
  val syndrome = Reg(Bool())
  val toV = out port Vec.fill(deg)(Flow(Bits(chk_msg_len bits)))
  val cToV = new CToV(relayparams, deg)
  cToV.inputs.valid := (state === State.start_computing_cToV)
  cToV.inputs.payload.syndrome := syndrome
  cToV.inputs.raw_messages := fromV
  val cToVDelays = cToV.delays
  val neighbor_decisions = in port Vec.fill(deg)(Bool())
  val satisfied = out port Reg(Bool())
  satisfied := (neighbor_decisions.xorR === syndrome)
  when(state === State.loading_inputs) {
    syndrome := in_syndrome
  }
  for(i <- 0 until deg) {
    toV(i).valid := cToV.output.valid
    toV(i).payload := cToV.output.payload(i).asBits
  }
}
