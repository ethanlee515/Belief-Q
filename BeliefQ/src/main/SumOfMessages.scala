package beliefq

import spinal.core._
import spinal.lib._

class SumOfMessages(params: BeliefQParams, n: Int) extends Component {
  import params._
  val messages = in port Vec.fill(n)(message_t())
  val result = out port message_t()
  var delays = 1
  val sz = (n + 1) / 2
  var terms = Reg(Vec.fill(sz)(message_t()))
  for(j <- 0 until sz) {
    if(2 * j + 1 != messages.size) {
      terms(j) := (messages(2 * j) + messages(2 * j + 1)).truncated
    } else {
      terms(j) := messages(2 * j)
    }
  }
  while(terms.size != 1) {
    val new_sz = (terms.size + 1) / 2
    var next_terms = Reg(Vec.fill(new_sz)(message_t()))
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
