package beliefq

import spinal.core._
import spinal.lib._

class Edge extends Component {
  // TODO parametrize the widths of everything
  val vToC = out port Reg(AFix.SQ(8 bits, 8 bits))
  val cToV = out port Reg(AFix.SQ(8 bits, 8 bits))
  val toC = in port Flow(AFix.SQ(8 bits, 8 bits))
  val toV = in port Flow(AFix.SQ(8 bits, 8 bits))
  when(toC.valid) {
    vToC := toC.payload
  }
  when(toV.valid) {
    cToV := toV.payload
  }
}
