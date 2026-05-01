package beliefq
package relay

class Relay[V, F](
    params: RelayParams,
    var_labels: Set[V],
    chk_labels: Set[F],
    edges: Set[(V, F)],
  ) extends BeliefQ[V, F](
    params,
    var_labels,
    chk_labels,
    edges,
    (params, _, deg, seed) => new RelayVariable(params, deg, seed),
    (params, deg) => new MinSumCheck(params, deg)
  )
