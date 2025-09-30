package beliefq
package test

import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.{Report, Rtl}
import spinal.lib.eda.bench.Target
import spinal.lib.eda.bench.Bench
import spinal.lib.eda.xilinx.VivadoFlow

object XilinxRfsocTarget {
  def apply(FMax: HertzNumber = 500 MHz, vivadoPath: String = "/opt/Xilinx/Vivado/2024.2/bin/"): Seq[Target] = {
    return List(new Target {
      override def getFamilyName(): String = "Virtex UltraScale+"
      override def synthesise(rtl: Rtl, workspace: String): Report = {
        VivadoFlow(
          frequencyTarget = FMax,
          vivadoPath = vivadoPath,
          workspacePath = workspace + "_area",
          rtl = rtl,
          family = getFamilyName(),
          device = "xczu49dr-ffvf1760-2-e"
        )
      }
    })
  }
}

object VivadoBench extends App {
  val params = new BeliefQParams()
  val var_labels = (0 until SimData.num_vars).toSet
  val chk_labels = (0 until SimData.num_checks).toSet
  val rtl = Rtl(
    SpinalVerilog(new VanillaBP(params, var_labels, chk_labels, SimData.edges))
  )
  Bench(List(rtl), XilinxRfsocTarget(1000 MHz), "./build/")
}
