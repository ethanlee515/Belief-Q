package beliefq

import spinal.core._
import spinal.lib._

class Check(params: BeliefQParams, deg: Int) extends Component {
  import params._
  val fromV = in port Vec.fill(deg)(message_t())
  val toV = out port Vec.fill(deg)(Flow(message_t))
  for(f <- toV) {
    f.setIdle()
  }
}
