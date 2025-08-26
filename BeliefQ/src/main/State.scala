package beliefq

import spinal.core._
import spinal.lib._

object State extends SpinalEnum {
    val idle, computing_vToC, computing_cToV, checking_decision, result_valid = newElement()
}
