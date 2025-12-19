package beliefq
package relay

import spinal.core._
import spinal.lib._

object State extends SpinalEnum {
  val idle, loading_inputs,
    start_computing_bias, computing_bias,
    start_summing_messages, summing_messages,
    variables_decide,
    checks_decide,
    checking_decision,
    start_computing_cToV, computing_cToV,
    result_valid, rerandomize_weights,
    failed = newElement()
}
