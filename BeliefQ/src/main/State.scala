package beliefq

import spinal.core._
import spinal.lib._

object State extends SpinalEnum {
  val idle, loading_inputs,
    start_computing_vToC, computing_vToC,
    start_computing_cToV, computing_cToV,
    start_decide, deciding,
    /* start_checking_decision, */ checking_decision,
    result_valid = newElement()
}
