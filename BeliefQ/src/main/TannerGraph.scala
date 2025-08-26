package beliefq

import spinal.core._
import spinal.lib._

// Set of edges is fine
// No need for the sparse matrix formalism
class TannerGraph[V, C](
    params: BeliefQParams,
    var_labels: Set[V],
    chk_labels: Set[C],
    edge_labels: Set[(V, C)]) extends Component {
  import params._
  /* -- IO -- */
  val state = in port State()
  // Instantiating the graph
  def get_neighboring_checks(variable: V) : Set[C] = {
    edge_labels.collect{case (v, f) if (v == variable) => f}
  }.toSet
  def get_neighboring_variables(check: C) : Set[V] = {
    edge_labels.collect{case (v, f) if (f == check) => v}
  }.toSet
  def deg_var(v: V) : Int = {
    get_neighboring_checks(v).size
  }
  def deg_check(c: C) : Int = {
    get_neighboring_variables(c).size
  }
  val variables = {
    for(v <- var_labels)
      yield (v -> new Variable(params, deg_var(v)))
  }.toMap
  val checks = {
    for(f <- chk_labels)
      yield (f -> new Check(params, deg_check(f)))
  }.toMap
  val edges = {
    for(e <- edge_labels)    
      yield (e -> new Edge(params))
  }.toMap
  for(v <- var_labels) {
    val variable = variables(v)
    val checks = get_neighboring_checks(v).toSeq
    for(i <- 0 until checks.length) {
      val edge_label = (v, checks(i))
      val edge = edges(edge_label)
      edge.fromV << variable.toC(i)
      variable.fromC(i) := edge.toV
    }
  }
  for(c <- chk_labels) {
    val check = checks(c)
    val vars = get_neighboring_variables(c).toSeq
    for(i <- 0 until vars.length) {
      val edge = edges(vars(i), c)
      edge.fromC << check.toV(i)
      check.fromV(i) := edge.toC
    }
  }
}
