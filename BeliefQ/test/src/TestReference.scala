package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.reference.VanillaBP

object TestReference extends TestSuite {
  def tests = Tests {
    test("Scala reference matches Rust output") {
      for(i <- 0 until 10000) {
        println(f"checking data: ${i}")
        if(SimData.is_converged(i) && !SimData.strange_indices.contains(i)) {
          val var_labels = (0 until SimData.num_vars).toSet
          val factor_labels = (0 until SimData.num_checks).toSet
          val syndromes = {
            for(j <- 0 until SimData.num_checks) yield {
              j -> SimData.syndromes_batch(i)(j)
            }
          }.toMap
          val log_priors = {
            for(j <- 0 until SimData.num_vars) yield {
              j -> SimData.log_priors(j)
            }
          }.toMap
          val vanillaBP = new VanillaBP(var_labels, factor_labels, SimData.edges, syndromes, log_priors)
          val results : Option[Map[Int, Boolean]] = vanillaBP.doBP(500)
          val expected_results = SimData.ehat_bp(i)
          results match {
            case Some(res) => {
              for(j <- 0 until SimData.num_vars) {
                assert(res(j) == expected_results(j))
              }
            }
            case None => {
              assert(false)
            }
          }
        }
      }
    }
  }
}

