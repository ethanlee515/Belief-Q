package beliefq
package relay

import spinal.core._
import spinal.lib._

class BeliefQ[V, F](
    params: BeliefQParams,
    var_labels: Set[V],
    chk_labels: Set[F],
    edges: Set[(V, F)],
    make_var: (BeliefQParams, Int, BigInt) => Variable,
    make_chk: (BeliefQParams, Int) => Check,
  ) extends Component {
  /* -- IO -- */
  import params._
  val inputs = slave Stream(BeliefQInputs(params, var_labels, chk_labels))
  val outputs = out port Flow(BeliefQOutputs(var_labels))
  val failed = out port Bool()

  val cached_initial_priors = {
    for(v <- var_labels) yield {
      v -> Reg(initial_priors_t())
    }
  }.toMap
  val cached_syndromes = {
    for(f <- chk_labels) yield {
      f -> Reg(Bool())
    }
  }.toMap

  val graph = new TannerGraph(params, var_labels, chk_labels, edges, make_var, make_chk)
  val controller = new Controller(params, graph)
  val quality_eval = new QualityEval(params, var_labels)

  val idle = controller.state === State.idle
  val output_ready = quality_eval.corrections_out_valid
  inputs.ready := idle && output_ready
  failed := (controller.state === State.failed)

  when(inputs.fire) {
    for(v <- var_labels) {
      cached_initial_priors(v) := inputs.payload.initial_priors(v)
    }
    for(f <- chk_labels) {
      cached_syndromes(f) := inputs.payload.syndromes(f)
    }
  }

  val delayed_inputs_fire = Reg(Bool()) init(False)
  delayed_inputs_fire := inputs.fire
  controller.start := delayed_inputs_fire

  for(v <- var_labels) {
    graph.priors_in(v) := cached_initial_priors(v)
    quality_eval.initial_priors(v) := cached_initial_priors(v)
  }
  for(c <- chk_labels) {
    graph.in_syndromes(c) := cached_syndromes(c)
  }
  graph.state := controller.state
  controller.converged := graph.converged

  quality_eval.rst := (controller.state === State.loading_inputs)
  quality_eval.corrections_in_valid := (controller.state === State.result_valid)
  for(v <- var_labels) {
    quality_eval.corrections_in(v) := graph.corrections(v)
  }

  outputs.valid := idle && output_ready
  for(v <- var_labels) {
    outputs.corrections(v) := quality_eval.corrections_out(v)
  }
}
