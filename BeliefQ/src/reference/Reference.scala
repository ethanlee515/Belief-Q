package beliefq
package reference

class Reference[V, F](
  max_iters: Int,
  var_labels: Set[V],
  factor_labels: Set[F],
  edges: Set[(V, F)]) {
  var messages = {
    for(e <- edges) yield {
      e -> (0 : Double)
    }
  }.toSet
}
