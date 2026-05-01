package beliefq
package relay

class VanillaBP[V, F](
    params: RelayParams,
    var_labels: Set[V],
    chk_labels: Set[F],
    edges: Set[(V, F)],
  ) extends BeliefQ[V, F](
    params,
    var_labels,
    chk_labels,
    edges,
    (params, _, deg, _) => new VanillaVariable(params, deg),
    (params, deg) => new RelayCheck(params, deg)
  )
