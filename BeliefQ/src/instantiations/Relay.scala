package beliefq
package relay

import spinal.lib._
import spinal.core._

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
    (params, _, deg, seed) => {
      val v = new RelayVariable(params, deg)
      v.seed := B(seed, 64 bits)
      v
      },
    (params, deg) => new MinSumCheck(params, deg)
  )
