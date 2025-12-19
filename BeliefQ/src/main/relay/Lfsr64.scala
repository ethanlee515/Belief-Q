package beliefq
package relay

import spinal.core._
import spinal.lib._

case class Lfsr64(params: BeliefQParams, seed: BigInt) extends Component {
  import params._
  val lfsr = Reg(Bits(64 bits)) init seed
  val feedback = lfsr(63) ^ lfsr(62) ^ lfsr(60) ^ lfsr(59)
  lfsr := lfsr(62 downto 0) ## feedback
  val rng_norm1 = out port AFix.UQ(0 bits, message_fractional_precision bits)
  rng_norm1.assignFromBits(lfsr(0 until message_fractional_precision))
}
