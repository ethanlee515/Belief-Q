package beliefq
package vanilla

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

class VanillaBP[V, F](params: BeliefQParams,
    var_labels: Set[V],
    chk_labels: Set[F],
    edges: Set[(V, F)],
  ) extends Component {
  /* -- IO -- */
  import params._
  val inputs = slave Stream(BeliefQInputs(params, var_labels, chk_labels))
  val cached_initial_priors = {
    for(v <- var_labels) yield {
      v -> Reg(message_t())
    }
  }.toMap
  val cached_syndromes = {
    for(f <- chk_labels) yield {
      f -> Reg(Bool())
    }
  }.toMap
  when(inputs.fire) {
    for(v <- var_labels) {
      cached_initial_priors(v) := inputs.payload.initial_priors(v)
    }
    for(f <- chk_labels) yield {
      cached_syndromes(f) := inputs.payload.syndromes(f)
    }
  }
  val outputs = out port Flow(BeliefQOutputs(var_labels))
  val graph = new TannerGraph(params, var_labels, chk_labels, edges)
  val controller = new Controller(graph)
  inputs.ready := (controller.state === State.idle)
  val delayed_inputs_valid = Reg(Bool()) init(False)
  delayed_inputs_valid := inputs.valid
  controller.start := delayed_inputs_valid
  for(v <- var_labels) {
    graph.priors_in(v) := cached_initial_priors(v)
  }
  for(c <- chk_labels) {
    graph.in_syndromes(c) := cached_syndromes(c)
  }
  graph.state := controller.state
  controller.converged := graph.converged
  outputs.valid := (controller.state === State.result_valid)
  for(v <- var_labels) {
    outputs.corrections(v) := graph.corrections(v)
  }
}
