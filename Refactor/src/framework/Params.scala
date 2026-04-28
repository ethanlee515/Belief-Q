package beliefq

import spinal.core._

case class BeliefQParams() {
  val var_msg_len = 13
  val chk_msg_len = 13
  val initial_priors_t = HardType(AFix.SQ(8 bits, 4 bits))
}
