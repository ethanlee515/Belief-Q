// Converting surface code geometry to Tanner graph

package beliefq
import spinal.core._
import spinal.lib._

trait Variable2D
case class DataError2D(x: Int, y: Int) extends Variable2D
case class MeasError2D(x: BigDecimal, y: BigDecimal) extends Variable2D
case class Factor2D(x: BigDecimal, y: BigDecimal)

case class Variable3D(t: Int, variable2d: Variable2D)
case class Factor3D(t: Int, factor2d: Factor2D)

// This is actually rotated surface code
// TODO draw or ref image
class SurfaceCodeDecoder(
  params: BeliefQParams,
  distance: Int, num_meas: Int) extends Component {
  require(distance > 0 && distance % 2 == 1)
  // 2D stuff first
  val dataLocations2D = {
    for(x <- 0 until distance;
        y <- 0 until distance) yield (x, y)
  }.toSet
  val dataErrors2D = dataLocations2D.map(loc => {
    val (x, y) = loc
    DataError2D(x, y) : Variable2D
  })
  val syndromeLocations2D = {
    for(i <- 0 until distance + 1;
        j <- 0 until distance / 2) yield {
      val half = BigDecimal("0.5")
      val offset = { if(i % 2 == 0) half else (half + 1) }
      (i - half, offset + 2 * j)
    }
  }.toSet
  val measErrors2D : Set[Variable2D] = syndromeLocations2D.map {
    case (x, y) => MeasError2D(x, y) : Variable2D
  }
  val variables2D = dataErrors2D union measErrors2D
  val factors2D = syndromeLocations2D.map(Factor2D.tupled)
  /** For a rotated-code check at (x, y), return adjacent integer data qubits. */
  def physical_of(f: Factor2D): Set[(Int, Int)] = {
    val Factor2D(x, y) = f
    val half = BigDecimal("0.5")
    val candidates = {
      for(a <- Seq(-1, 1);
          b <- Seq(-1, 1))
        yield ((x + a * half).intValue, (y + b * half).intValue)
    }
    candidates.filter(dataLocations2D.contains).toSet
  }
  val dataEdges2D = {
    for(factor <- factors2D;
        (x, y) <- physical_of(factor)) yield {
      (DataError2D(x, y), factor)
    } : (Variable2D, Factor2D)
  }.toSet
  val measEdges2D = {
    for((x, y) <- syndromeLocations2D) yield {
      (MeasError2D(x, y), Factor2D(x, y))
    } : (Variable2D, Factor2D)
  }.toSet
  val edges2D = dataEdges2D union measEdges2D
  // product with 3D
  val variables = {
    for(variable <- variables2D;
        t <- 0 until num_meas) yield Variable3D(t, variable)
  }.toSet
  val factors = {
    for(factor <- factors2D;
        t <- 0 until num_meas) yield Factor3D(t, factor)
  }.toSet
  val edges = {
    for((v2, f2) <- edges2D;
        t <- 0 until num_meas) yield {
      (Variable3D(t, v2), Factor3D(t, f2))
    }
  }.toSet
  val beliefq = new BeliefQ[Variable3D, Factor3D](params, variables, factors, edges)
}
