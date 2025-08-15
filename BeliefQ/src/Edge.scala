package beliefq

import spinal.core._
import spinal.lib._

class Edge extends Component {
  // TODO parametrize the widths of everything
  val vToC = Reg(AFix.SQ(8 bits, 8 bits))
  val cToV = Reg(AFix.SQ(8 bits, 8 bits))
  val toC = in port Flow(AFix.SQ(8 bits, 8 bits))
  val toV = in port Flow(AFix.SQ(8 bits, 8 bits))
  val fromC = out port AFix.SQ(8 bits, 8 bits)
  val fromV = out port AFix.SQ(8 bits, 8 bits)
  when(toC.valid) {
    vToC := toC.payload
  }
  when(toV.valid) {
    cToV := toV.payload
  }
  fromC := vToC
  fromV := cToV
}
