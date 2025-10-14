package beliefq
package dmem

import spinal.core._
import spinal.lib._

object State extends SpinalEnum {
  val idle, loading_inputs,
    start_summing_messages, summing_messages,
    variables_decide,
    checks_decide,
    checking_decision,
    start_computing_cToV, computing_cToV,
    result_valid = newElement()
}
