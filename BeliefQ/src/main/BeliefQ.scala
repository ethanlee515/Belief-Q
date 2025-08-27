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
  }
}

case class BeliefQOutputs[V](var_labels: Set[V]) extends Bundle {
  val corrections = {
    for(v <- var_labels) yield {
      v -> Bool()
    }
  }
}

class BeliefQ[V, F](params: BeliefQParams,
    var_labels: Set[V],
    factor_labels: Set[F],
    edges: Set[(V, F)],
  ) extends Component {
  /* -- IO -- */
  val inputs = in port Flow(BeliefQInputs(params, var_labels, factor_labels))
  val outputs = out port Flow(BeliefQOutputs(var_labels))
  val controller = new Controller()
  val graph = new TannerGraph(params, var_labels, factor_labels, edges)
  graph.state := controller.state
  for(v <- var_labels) {
    graph.in_priors(v) := inputs.initial_priors(v)
  }
}
