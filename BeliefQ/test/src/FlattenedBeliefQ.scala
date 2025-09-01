package beliefq
package test

import spinal.core._
import spinal.lib._

class FlattenedBeliefQ[V, F](params: BeliefQParams,
    var_labels: Set[V],
    factor_labels: Set[F],
    edges: Set[(V, F)],
  ) extends Component {
  /* -- IO -- */
  import params._
  val beliefq = new BeliefQ(params, var_labels, factor_labels, edges)
  val input_valid = in port Bool()
  val initial_priors = {
    for(v <- var_labels) yield {
      v -> (in port message_t())
    }
  }.toMap
  val syndromes = {
    for(f <- factor_labels) yield {
      f -> (in port Bool())
    }
  }.toMap
  beliefq.inputs.valid := input_valid
  for(v <- var_labels) {
    beliefq.inputs.payload.initial_priors(v) := initial_priors(v)
  }
  for(f <- factor_labels) {
    beliefq.inputs.payload.syndromes(f) := syndromes(f)
  }
  val output_valid = out port Bool()
  output_valid := beliefq.outputs.valid
  val corrections = {
    for(v <- var_labels) yield {
      v -> (out port Bool())
    }
  }.toMap
  for(v <- var_labels) {
    corrections(v) := beliefq.outputs.payload.corrections(v)
  }
}
