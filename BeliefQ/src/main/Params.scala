package beliefq

import spinal.core._

case class BeliefQParams(
  message_integral_size : Int = 8,
  message_fractional_precision : Int = 8
) {
  val message_t = HardType(AFix.SQ(message_integral_size bits, message_fractional_precision bits))
  val unsigned_msg_t = HardType(AFix.UQ(message_integral_size bits, message_fractional_precision bits))
}
