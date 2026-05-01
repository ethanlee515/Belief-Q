// Converting surface code geometry to Tanner graph

package beliefq
import spinal.core._
import spinal.lib._

sealed trait Variable2D
case class DataError2D(x: Int, y: Int) extends Variable2D
case class MeasError2D(x: BigDecimal, y: BigDecimal) extends Variable2D
// 3D
case class Variable3D(t: Int, variable2d: Variable2D)
// First round: syndrome measurement
// After that: Detector click
case class Factor(t: Int, x: BigDecimal, y: BigDecimal)

// This is actually rotated surface code
// TODO draw or ref image
class SurfaceCodeGeometry(distance: Int, num_meas: Int) {
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
  /** For a rotated-code check at (x, y), return adjacent integer data qubits. */
  def physical_of(x: BigDecimal, y: BigDecimal): Set[(Int, Int)] = {
    val half = BigDecimal("0.5")
    val candidates = {
      for(a <- Seq(-1, 1);
          b <- Seq(-1, 1))
        yield ((x + a * half).intValue, (y + b * half).intValue)
    }
    candidates.filter(dataLocations2D.contains).toSet
  }
  val dataEdges2D = {
    for((x, y) <- syndromeLocations2D;
        (xdata, ydata) <- physical_of(x, y)) yield {
      (DataError2D(xdata, ydata), (x, y))
    }
  }.toSet
  // product with time dimension
  val variables = {
    for(variable <- variables2D;
        t <- 0 until num_meas) yield Variable3D(t, variable)
  }.toSet
  val factors = {
    for((x, y) <- syndromeLocations2D;
        t <- 0 until num_meas) yield Factor(t, x, y)
  }.toSet
  val dataEdges3D = {
    for((v, (x, y)) <- dataEdges2D;
        t <- 0 until num_meas) yield {
      (Variable3D(t, v), Factor(t, x, y))
    }
  }.toSet
  val detectorEdges0 = {
    for(t <- 0 until num_meas;
        (x, y) <- syndromeLocations2D) yield {
      val v = Variable3D(t, MeasError2D(x, y))
      val f = Factor(t, x, y)
      (v, f)
    }
  }.toSet
  val detectorEdges1 = {
    for(t <- 1 until num_meas;
        (x, y) <- syndromeLocations2D) yield {
      val v = Variable3D(t - 1, MeasError2D(x, y))
      val f = Factor(t, x, y)
      (v, f)
    }
  }.toSet
  val edges = dataEdges3D union detectorEdges0 union detectorEdges1
}

/*
object CompileVerilog extends App {
  val d = 3
  val num_meas = 3
  val geo = new SurfaceCodeGeometry(d, num_meas)
  val params = new BeliefQParams()
  SpinalVerilog(new BeliefQ(params, geo.variables, geo.factors, geo.edges))
}
*/

/*
class SurfaceCodeDecoder(
  params: BeliefQParams,
  distance: Int, num_meas: Int) extends Component {
  require(distance > 0 && distance % 2 == 1)
  val geo = new SurfaceCodeGeometry(distance, num_meas)
  val beliefq = new BeliefQ[Variable3D, Factor](params, geo.variables, geo.factors, geo.edges)
}
*/
