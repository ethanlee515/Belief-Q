package beliefq
package reference

class VanillaBP[V, F](
  var_labels: Set[V],
  factor_labels: Set[F],
  edges: Set[(V, F)],
  log_priors: Map[V, Double]) {
  def doBP(max_iters: Int, syndromes: Map[F, Boolean]) : Map[V, Boolean] = {
    var vToCs = {
      for(e <- edges) yield {
        val (v, c) = e
        e -> log_priors(v)
      }
    }.toSet
    var cToVs = {
      for(e <- edges) yield {
        e -> (0 : Double)
      }
    }.toSet
    for(i <- 0 until max_iters) {
      cToVs = CToV.compute(syndromes, vToCs)
      // TODO check converge. if so, return

      vToCs = VToC.compute(log_priors, cToVs)
    }
    throw new Exception("did not converge")
  }
}
