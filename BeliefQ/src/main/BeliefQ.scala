package beliefq
import spinal.core._
import spinal.lib._

class BeliefQ[V, F](
    params: BeliefQParams,
    var_labels: Set[V],
    factor_labels: Set[F],
    edges: Set[(V, F)]) extends Component {
  val controller = new Controller()
  val graph = new TannerGraph(params, var_labels, factor_labels, edges)
  graph.state := controller.state
}
