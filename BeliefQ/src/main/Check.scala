package beliefq

import spinal.core._
import spinal.lib._

class Check(params: BeliefQParams, deg: Int) extends Component {
  import params._
  val state = in port State()
  val fromV = in port Vec.fill(deg)(message_t())
  val in_syndrome = in port Bool()
  val syndrome = Reg(Bool())
  val toV = out port Vec.fill(deg)(Flow(message_t))
  when(state === State.loading_inputs) {
    syndrome := in_syndrome
  }
  for(f <- toV) {
    f.setIdle()
  }
}
