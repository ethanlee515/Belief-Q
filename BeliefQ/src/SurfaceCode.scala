// Converting surface code geometry to Tanner graph

package beliefq
import spinal.core._
import spinal.lib._

sealed trait VariableLabel
final case class DataError(k: Int, i: Int, j: Int)
// Syndrome are indexed by the (i, j) to the lower right of them
// TODO should draw this
final case class MeasurementError(k: Int, i: Int, j: Int)

final case class FactorLabel(k: Int, i: Int, j: Int)

// TODO now what?
// I guess take distance and output a `BeliefQ` component based on it?

class SurfaceCodeDecoder(distance: Int) extends Component {
  val v = out UInt(8 bits)
  v := distance
}