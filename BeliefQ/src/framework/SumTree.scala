package beliefq
package relay

import spinal.core._
import spinal.lib._

class SumTree[T <: AFix](
    dataType: HardType[T],
    n: Int) extends Component {
  val inputs = in port Vec.fill(n)(dataType())
  val result = out port dataType()
  var delays = 1
  val sz = (n + 1) / 2
  var terms = Reg(Vec.fill(sz)(dataType()))
  for(j <- 0 until sz) {
    if(2 * j + 1 != inputs.size) {
      terms(j) := (inputs(2 * j) + inputs(2 * j + 1)).truncated
    } else {
      terms(j) := inputs(2 * j)
    }
  }
  while(terms.size != 1) {
    val new_sz = (terms.size + 1) / 2
    var next_terms = Reg(Vec.fill(new_sz)(dataType()))
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
