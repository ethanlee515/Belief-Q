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
  val corrections_in_valid = in Bool()
  /*
  val corrections_out = {
    for(v <- var_labels) yield {
      v -> (out port Bool())
    }
  }.toMap
  val corrections_out_valid = out Bool()
  */
  val best_decoding_quality = out port Reg(message_t())
  /* -- logic -- */
  val vars_seq = var_labels.toSeq
  val len = vars_seq.length
  val sumOfMessages = new SumOfMessages(params, len)
  val delays = sumOfMessages.delays + 2
  val counter = Reg(UInt(8 bits))
  when(corrections_in_valid) {
    counter := 1
  } elsewhen(counter < sumOfMessages.delays) {
    counter := counter + 1
  } otherwise {
    counter := 0
    // TODO
    best_decoding_quality := sumOfMessages.result
  }

  val filtered_messages = Vec.fill(len)(Reg(message_t()))
  for(i <- 0 until vars_seq.length) {
    val v = vars_seq(i)
    when(corrections_in(v)) {
      filtered_messages(i) := initial_priors(v)
    } otherwise {
      filtered_messages(i) := BigDecimal(0)
    }
  }
  sumOfMessages.messages := filtered_messages
}
