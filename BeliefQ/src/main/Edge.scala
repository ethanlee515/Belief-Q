package beliefq

import spinal.core._
import spinal.lib._

class Edge(params: BeliefQParams) extends Component {
  import params._
  val state = in port State()
  val vToC = Reg(message_t)
  val cToV = Reg(message_t)
  val fromC = in port Flow(message_t)
  val fromV = in port Flow(message_t)
  val toC = out port message_t
  val toV = out port message_t
  val decision_in = in port Bool()
  val decision = out port Bool()
  decision := decision_in
  when(state === State.loading_inputs) {
    cToV := BigDecimal(0)
  }
  when(state === State.computing_cToV && fromC.valid) {
    cToV := fromC.payload
  }
  when(state === State.computing_vToC && fromV.valid) {
    vToC := fromV.payload
  }
  toC := vToC
  toV := cToV
  when(state === State.loading_inputs) {
    vToC := BigDecimal(0)
  }
}
