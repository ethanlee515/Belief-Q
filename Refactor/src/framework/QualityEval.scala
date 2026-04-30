package beliefq
package relay

import spinal.core._
import spinal.lib._

class QualitySum(
    params: BeliefQParams,
    n: Int) extends Component {
  import params._
  val terms_in = in port Vec.fill(n)(quality_t())
  val result = out port quality_t()
  var delays = 1
  val sz = (n + 1) / 2
  var terms = Reg(Vec.fill(sz)(quality_t()))
  for(j <- 0 until sz) {
    if(2 * j + 1 != terms_in.size) {
      terms(j) := (terms_in(2 * j) + terms_in(2 * j + 1)).truncated
    } else {
      terms(j) := terms_in(2 * j)
    }
  }
  while(terms.size != 1) {
    val new_sz = (terms.size + 1) / 2
    var next_terms = Reg(Vec.fill(new_sz)(quality_t()))
    for(j <- 0 until new_sz) {
      if(2 * j + 1 != terms.size) {
        next_terms(j) := (terms(2 * j) + terms(2 * j + 1)).truncated
      } else {
        next_terms(j) := terms(2 * j)
      }
    }
    terms = next_terms
    delays += 1
  }
  result := terms(0)
}

class QualityEval[V](
    params: BeliefQParams,
    var_labels: Set[V]) extends Component {
  import params._
  /* -- IO -- */
  val initial_priors = {
    for(v <- var_labels) yield {
      v -> (in port initial_priors_t())
    }
  }.toMap
  val corrections_in = {
    for(v <- var_labels) yield {
      v -> (in port Bool())
    }
  }.toMap
  val current_corrections = {
    for(v <- var_labels) yield {
      v -> Reg(Bool())
    }
  }.toMap
  val corrections_in_valid = in Bool()
  val corrections_out = {
    for(v <- var_labels) yield {
      v -> (out port Reg(Bool()) init(False))
    }
  }.toMap
  val corrections_out_valid = out Bool()
  val best_decoding_quality = out port Reg(quality_t()) init(quality_t().maxValue)
  val rst = in Bool()
  /* -- internal data -- */
  val vars_seq = var_labels.toSeq
  val len = vars_seq.length
  val counter = Reg(UInt(8 bits)) init(0)
  val filtered_priors = Vec.fill(len)(Reg(quality_t()))
  val quality_sum = new QualitySum(params, len)
  /* -- logic -- */
  when(rst) {
    best_decoding_quality := quality_t().maxValue
    counter := 0
  }
  corrections_out_valid := (counter === 0)
  for(i <- 0 until vars_seq.length) {
    val v = vars_seq(i)
    when(corrections_in(v)) {
      filtered_priors(i) := initial_priors(v)
    } otherwise {
      filtered_priors(i) := BigDecimal(0)
    }
  }
  quality_sum.terms_in := filtered_priors
  when(corrections_in_valid) {
    counter := 1
    for(v <- var_labels) {
      current_corrections(v) := corrections_in(v)
    }
  } elsewhen(counter =/= 0 && counter < quality_sum.delays + 1) {
    counter := counter + 1
  } elsewhen(counter === quality_sum.delays + 1) {
    counter := 0
    when(quality_sum.result < best_decoding_quality) {
      best_decoding_quality := quality_sum.result
      for(v <- var_labels) {
        corrections_out(v) := current_corrections(v)
      }
    }
  }
  val delays = quality_sum.delays + 2
}
