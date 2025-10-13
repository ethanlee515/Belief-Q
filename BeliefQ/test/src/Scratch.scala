package beliefq
package test

object Scratch extends App {
//  println(f"num data = ${SimData.ehat_bp.length}")
//  println(f"num vars = ${SimData.num_vars}, num checks = ${SimData.num_checks}")
  var diverge_count = 0
  for(i <- 0 until 10000) {
    if(!SimData.is_dmem_converged(i)) {
      diverge_count += 1
    }
  }
  println(f"num diverged out of first 10k: ${diverge_count}")
}
