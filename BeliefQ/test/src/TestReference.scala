package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.reference.VanillaBP
import beliefq.reference.DMemBP

object TestReference extends TestSuite {
  def tests = Tests {
    test("Scala reference matches Rust output") {
      for(i <- 0 until 100) {
        // println(f"checking data: ${i}")
        if(SimData.is_converged(i) && !SimData.strange_indices.contains(i)) {
          val var_labels = (0 until SimData.num_vars).toSet
          val chk_labels = (0 until SimData.num_checks).toSet
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
          val vanillaBP = new VanillaBP(var_labels, chk_labels, SimData.edges, syndromes, log_priors)
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
/*
    test("DMem Scala reference matches Rust output") {
      for(i <- 0 until 1000) {
        // println(f"checking data: ${i}")
        val bads = Set(45, 64, 201, 269, 430, 493, 552, 632)
        if(SimData.is_dmem_converged(i)) {
          val var_labels = (0 until SimData.num_vars).toSet
          val chk_labels = (0 until SimData.num_checks).toSet
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
          val gammas = {
            for(j <- 0 until SimData.num_vars) yield {
              j -> SimData.gammas(j)
            }
          }.toMap
          val bp = new DMemBP(var_labels, chk_labels, SimData.edges, syndromes, gammas, log_priors)
          val results : Option[Map[Int, Boolean]] = bp.doBP(500)
          val expected_results = SimData.ehat_dmembp(i)

          if(bads.contains(i)) {
            var is_bad = false
            results match {
              case Some(res) => {
                for(j <- 0 until SimData.num_vars) {
                  if(res(j) != expected_results(j)) {
                    is_bad = true
                  }
                }
              }
              case None => {
                is_bad = true
              }
            }
            if(is_bad) {
              println(f"#${i} is bad")
            }
          } else {
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
    }*/
  }
}

