package beliefq

import spinal.core._
import spinal.lib._

abstract class Variable extends Component {
  val fromC_raw : Vec[Bits]
  val toC_raw : Vec[Flow[Bits]]
  val iter0 : Bool
  val prior_in : AFix
  val state : Data
  val decision : Bool
  val vToCDelays : Int
}
