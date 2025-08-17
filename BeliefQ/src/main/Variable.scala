package beliefq

import spinal.core._
import spinal.lib._

class Variable(params: BeliefQParams, deg: Int) extends Component {
  import params._
  val fromC = in port Vec.fill(deg)(message_t())
  val toC = out port Vec.fill(deg)(Flow(message_t))
  for(f <- toC) {
    f.setIdle()
  }
}
