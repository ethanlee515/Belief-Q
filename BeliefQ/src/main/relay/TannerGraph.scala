package beliefq
package relay

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
  val start = in port Bool()
  val priors_in = {
    for(v <- var_labels) yield {
      v -> (in port message_t())
    }
  }.toMap
  val in_syndromes = {
    for(c <- chk_labels) yield {
      c -> (in port Bool())
    }
  }.toMap
  val corrections = {
    for(v <- var_labels) yield {
      v -> (out port Bool())
    }
  }.toMap
  val converged = out port Bool()
  // Instantiating the graph
  val iter0 = Reg(Bool()) init(False)
  when(state === State.loading_inputs) {
    iter0 := True
  }
  when(state === State.checking_decision) {
    iter0 := False
  }
  val geometry = new TannerGraphGeometry(/*params, */ var_labels, chk_labels, edge_labels)
  import geometry._
  val variables = {
    for(v <- var_labels) yield {
      val variable = new Variable(params, deg_var(v))
      variable.state := state
      variable.prior_in := priors_in(v)
      variable.iter0 := iter0
      v -> variable
    }
  }.toMap
  val checks = {
    for(f <- chk_labels) yield {
      val check = new Check(params, deg_check(f))
      check.state := state
      check.in_syndrome := in_syndromes(f)
      f -> check
    }
  }.toMap
  val is_loading = state === State.loading_inputs
  val edges = {
    for(e <- edge_labels) yield {
      val edge = new Edge(params)
      edge.loading_inputs := is_loading
      e -> edge
    }
  }.toMap
  for(v <- var_labels) {
    val variable = variables(v)
    val checks = get_neighboring_checks(v).toSeq
    for(i <- 0 until checks.length) {
      val edge_label = (v, checks(i))
      val edge = edges(edge_label)
      when(is_loading) {
        edge.fromV.push(priors_in(v))
      } otherwise {
        edge.fromV << variable.toC(i)
      }
      edge.decision_in := variable.decision
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
      check.neighbor_decisions(i) := edge.decision
    }
  }
  val check_satisfied = {
    for(c <- chk_labels) yield {
      val check = checks(c)
      check.satisfied
    }
  }
  converged := Vec(check_satisfied).andR
  for(v <- var_labels) {
    corrections(v) := variables(v).decision
  }
  val cToVDelays = checks.values.maxBy(_.cToVDelays).cToVDelays
  val sumMessageDelays = variables.values.maxBy(_.sumMessageDelays).sumMessageDelays
  val bias_delays = variables.values.maxBy(_.bias_delays).bias_delays
}
