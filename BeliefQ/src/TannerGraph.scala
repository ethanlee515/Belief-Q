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
  def get_neighboring_factors(variable: V) : Set[F] = {
    edges.collect{case (v, f) if (v == variable) => f}
  }.toSet
  def get_neighboring_variables(factor: F) : Set[V] = {
    edges.collect{case (v, f) if (f == factor) => v}
  }.toSet
  def deg_var(variable: V) : Int = {
    get_neighboring_factors(variable).size
  }
  def deg_factor(factor: F) : Int = {
    get_neighboring_variables(factor).size
  }
  // TODO iterate?
  val variables = {
    for(v <- var_labels)
      yield new Variable(deg_var(v))
  }
  val factors = {
    for(f <- factor_labels)
      yield new Factor(deg_factor(f))
  }



}
