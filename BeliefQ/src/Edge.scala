package beliefq

import spinal.core._
import spinal.lib._

class Edge(params: BeliefQParams) extends Component {
  import params._
  val vToC = Reg(message_t)
  val cToV = Reg(message_t)
  val fromC = in port Flow(message_t)
  val fromV = in port Flow(message_t)
  val toC = out port message_t
  val toV = out port message_t
  when(fromC.valid) {
    cToV := fromC.payload
  }
  when(fromV.valid) {
    vToC := fromV.payload
  }
  toC := vToC
  toV := cToV
}
