package beliefq

import spinal.core._
import spinal.lib._

class Edge(params: BeliefQParams) extends Component {
  import params._
  val loading_inputs = in port Bool()
  val var_msg_t = HardType(Bits(var_msg_len bits))
  val chk_msg_t = HardType(Bits(chk_msg_len bits))
  val vToC = Reg(var_msg_t)
  val cToV = Reg(chk_msg_t)
  val fromC = in port Flow(chk_msg_t)
  val fromV = in port Flow(var_msg_t)
  val toC = out port var_msg_t
  val toV = out port chk_msg_t
  val decision_in = in port Bool()
  val decision = out port Bool()
  decision := decision_in
  when(loading_inputs) {
    cToV := B(0)
  }
  when(fromC.valid) {
    cToV := fromC.payload
  }
  when(fromV.valid) {
    vToC := fromV.payload
  }
  toC := vToC
  toV := cToV
}
