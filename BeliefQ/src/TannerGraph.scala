package beliefq

import spinal.core._
import spinal.lib._

// Set of edges is fine
// No need for the sparse matrix formalism
class TannerGraph[V, F](
    var_labels: Set[V],
    factor_labels: Set[F],
    edges: Set[(V, F)]) extends Component {
  /* -- IO -- */
  val state = in port State()
  // Instantiating the graph
  def get_var_degree(v: V) : Int = {
    return 0
  }
  def get_factor_degree(f: F) : Int = {
    return 0
  }
  // TODO iterate?
}