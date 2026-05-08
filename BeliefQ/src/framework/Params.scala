package beliefq

import spinal.core._

class BeliefQParams() {
  val var_msg_len = 13
  val chk_msg_len = 13
  val initial_priors_t = HardType(AFix.SQ(8 bits, 4 bits))
  val quality_t = HardType(AFix.SQ(16 bits, 4 bits))
  val max_legs = 5
  val max_sols = 2
  val max_iters = 12
  val pipeline_converged = false
}
