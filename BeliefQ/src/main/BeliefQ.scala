package beliefq
import spinal.core._
import spinal.lib._

case class BeliefQInputs[V, F](params: BeliefQParams,
  var_labels: Set[V],
  factor_labels: Set[F],
) extends Bundle {
  import params._
  val initial_priors = {
    for(v <- var_labels) yield {
      v -> message_t()
    }
  }.toMap
  val syndromes = {
    for(f <- factor_labels) yield {
      f -> Bool()
    }
  }.toMap
}

case class BeliefQOutputs[V](var_labels: Set[V]) extends Bundle {
  val corrections = {
    for(v <- var_labels) yield {
      v -> Bool()
    }
  }.toMap
}

class BeliefQ[V, F](params: BeliefQParams,
    var_labels: Set[V],
    factor_labels: Set[F],
    edges: Set[(V, F)],
  ) extends Component {
  /* -- IO -- */
  val inputs = in port Flow(BeliefQInputs(params, var_labels, factor_labels))
  val cached_inputs = inputs.toReg()
  val outputs = out port Flow(BeliefQOutputs(var_labels))
  val graph = new TannerGraph(params, var_labels, factor_labels, edges)
  val controller = new Controller(graph)
  for(v <- var_labels) {
    graph.in_priors(v) := cached_inputs.initial_priors(v)
  }
  for(c <- factor_labels) {
    graph.in_syndromes(c) := cached_inputs.syndromes(c)
  }
  graph.state := controller.state
}
