package beliefq

import spinal.core._
import spinal.lib._

abstract class Check extends Component {
  val state : Data
  val fromV : Vec[Bits]
  val in_syndrome : Bool
  val toV : Vec[Flow[Bits]]
  val neighbor_decisions : Vec[Bool]
  val satisfied : Bool
  val cToVDelays : Int
}
