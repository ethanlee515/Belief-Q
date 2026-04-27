package beliefq
package relay

import spinal.core._
import spinal.lib._

class QualityEval[V](params: BeliefQParams, var_labels: Set[V]) extends Component {
  import params._
  /* -- IO -- */
  val initial_priors = {
    for(v <- var_labels) yield {
      v -> (in port message_t())
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
  val best_decoding_quality = out port Reg(message_t()) init(unsigned_msg_t().maxValue)
  val rst = in Bool()
  /* -- internal data -- */
  val vars_seq = var_labels.toSeq
  val len = vars_seq.length
  val counter = Reg(UInt(8 bits)) init(0)
  val filtered_messages = Vec.fill(len)(Reg(message_t()))
  val sumOfMessages = new SumOfMessages(params, len)
  /* -- logic -- */
  when(rst) {
    best_decoding_quality := unsigned_msg_t().maxValue
    counter := 0
  }
  corrections_out_valid := (counter === 0)
  for(i <- 0 until vars_seq.length) {
    val v = vars_seq(i)
    when(corrections_in(v)) {
      filtered_messages(i) := initial_priors(v)
    } otherwise {
      filtered_messages(i) := BigDecimal(0)
    }
  }
  sumOfMessages.messages := filtered_messages
  when(corrections_in_valid) {
    counter := 1
    for(v <- var_labels) {
      current_corrections(v) := corrections_in(v)
    }
  } elsewhen(counter =/= 0 && counter < sumOfMessages.delays + 1) {
    counter := counter + 1
  } elsewhen(counter === sumOfMessages.delays + 1) {
    counter := 0
    when(sumOfMessages.result < best_decoding_quality) {
      best_decoding_quality := sumOfMessages.result
      for(v <- var_labels) {
        corrections_out(v) := current_corrections(v)
      }
    }
  }
  val delays = sumOfMessages.delays + 2
}
