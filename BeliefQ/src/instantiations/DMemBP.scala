package beliefq
package relay

class DMemBP[V, F](
    params: RelayParams,
    var_labels: Set[V],
    chk_labels: Set[F],
    edges: Set[(V, F)],
    gammas: Map[V, BigDecimal],
  ) extends BeliefQ[V, F](
    params,
    var_labels,
    chk_labels,
    edges,
    (params, v, deg, _) => new DMemVariable(params, deg, gammas(v)),
    (params, deg) => new MinSumCheck(params, deg)
  )
