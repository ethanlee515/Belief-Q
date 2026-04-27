package beliefq
package relay

import spinal.core._
import spinal.lib._

class Check(params: BeliefQParams, deg: Int) extends Component {
  import params._
  val state = in port State()
  val fromV = in port Vec.fill(deg)(message_t())
  val in_syndrome = in port Bool()
  val syndrome = Reg(Bool())
  val toV = out port Vec.fill(deg)(Flow(message_t))
  val cToV = new CToV(params, deg)
  cToV.inputs.valid := (state === State.start_computing_cToV)
  cToV.inputs.payload.syndrome := syndrome
  cToV.inputs.messages := fromV
  val cToVDelays = cToV.delays
  val neighbor_decisions = in port Vec.fill(deg)(Bool())
  val satisfied = out port Reg(Bool())
  satisfied := (neighbor_decisions.xorR === syndrome)
  when(state === State.loading_inputs) {
    syndrome := in_syndrome
  }
  for(i <- 0 until deg) {
    toV(i).valid := cToV.output.valid
    toV(i).payload := cToV.output.payload(i)
  }
}
