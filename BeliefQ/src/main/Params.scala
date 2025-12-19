package beliefq

import spinal.core._

case class BeliefQParams(
  message_integral_size : Int = 8,
  message_fractional_precision : Int = 4,
  gamma_fractional_precision : Int = 4
) {
  val message_t = HardType(AFix.SQ(message_integral_size bits, message_fractional_precision bits))
  val unsigned_msg_t = HardType(AFix.UQ(message_integral_size bits, message_fractional_precision bits))
  val gamma_t = HardType(AFix.SQ(2 bits, gamma_fractional_precision bits))
  val max_legs = 5
  val max_sols = 2
  val max_iters = 6
}
