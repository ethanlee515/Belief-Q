package beliefq

import spinal.core._
import spinal.lib._


case class BeliefQInputs[V, F](params: BeliefQParams,
  var_labels: Set[V],
  chk_labels: Set[F],
) extends Bundle {
  import params._
  val initial_priors = {
    for(v <- var_labels) yield {
      v -> (in port message_t())
    }
  }.toMap
  val syndromes = {
    for(f <- chk_labels) yield {
      f -> (in port Bool())
    }
  }.toMap
}

case class BeliefQOutputs[V](var_labels: Set[V]) extends Bundle {
  val corrections = {
    for(v <- var_labels) yield {
      v -> (out port Bool())
    }
  }.toMap
}