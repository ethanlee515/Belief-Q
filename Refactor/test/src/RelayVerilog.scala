package beliefq
package test

import beliefq.relay._
import spinal.core._

object RelayVerilog extends App {
  val params = new RelayParams()
  val var_labels = (0 until SimData.num_vars).toSet
  val chk_labels = (0 until SimData.num_checks).toSet
  SpinalVerilog(new Relay(params, var_labels, chk_labels, SimData.edges))
}
