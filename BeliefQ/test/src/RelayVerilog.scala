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

object VanillaVerilog extends App {
  val params = new RelayParams()
  val var_labels = (0 until SimData.num_vars).toSet
  val chk_labels = (0 until SimData.num_checks).toSet
  SpinalVerilog(new VanillaBP(params, var_labels, chk_labels, SimData.edges))
}

object DMemVerilog extends App {
  val params = new RelayParams()
  val var_labels = (0 until SimData.num_vars).toSet
  val chk_labels = (0 until SimData.num_checks).toSet
  SpinalVerilog(new DMemBP(params, var_labels, chk_labels, SimData.edges, SimData.gammas))
}

object Bb144RelayVerilog extends App {
  val params = new RelayParams()
  val data = StimTannerData.bb144
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "./gen",
    oneFilePerComponent = false
  ).generate(
    new Relay(params, data.var_labels, data.chk_labels, data.edges)
    )
}

/*
object ColorCodeRelayVerilog extends App {
  val params = new RelayParams()
  val data = StimTannerData.colorCode
  SpinalVerilog(new Relay(params, data.var_labels, data.chk_labels, data.edges))
}
*/
