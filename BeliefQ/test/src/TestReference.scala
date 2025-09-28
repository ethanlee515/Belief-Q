package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.reference.VanillaBP

/*
class VanillaBP[V, F](
    var_labels: Set[V],
    factor_labels: Set[F],
    edges: Set[(V, F)],
    syndromes: Map[F, Boolean],
    log_priors: Map[V, BigDecimal]) {
    */

object TestReference extends TestSuite {
  def tests = Tests {
    test("Scala reference matches Rust output") {
      for(i <- 0 until 1000) {
        if(SimData.is_converged(i)) {
          val var_labels = (0 until SimData.num_vars).toSet
          val factor_labels = (0 until SimData.num_checks).toSet
          val syndromes = {
            for(c <- factor_labels) yield {
              c -> SimData.syndromes_batch(i)(c)
            }
          }.toMap
          val log_priors = {
            for(v <- var_labels) yield {
              // TODO Big Decimal...
              v -> BigDecimal(SimData.log_priors(v))
            }
          }.toMap
          val vanillaBP = new VanillaBP(var_labels, factor_labels, SimData.edges, syndromes, log_priors)
          val results = vanillaBP.doBP(100)
        }
      }
    }
  }
}

