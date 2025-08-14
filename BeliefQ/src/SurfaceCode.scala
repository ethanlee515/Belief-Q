// Converting surface code geometry to Tanner graph

package beliefq
import spinal.core._
import spinal.lib._

trait Variable2D
case class DataError2D(x: Int, y: Int) extends Variable2D
case class MeasError2D(x: BigDecimal, y: BigDecimal) extends Variable2D
case class Factor2D(x: BigDecimal, y: BigDecimal)

case class VariableLabel(t: Int, variable2d: Variable2D)
case class FactorLabel(t: Int, factor2d: Factor2D)

// This is actually rotated surface code
// TODO draw or ref image
class SurfaceCodeDecoder(distance: Int, num_meas: Int) extends Component {
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
  val measErrors2D = syndromeLocations2D.map(loc => {
    val (x, y) = loc
    MeasError2D(x, y) : Variable2D
  })
  val variables2D = dataErrors2D union measErrors2D
  val factors2D = syndromeLocations2D.map(Factor2D.tupled)
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
        loc <- physical_of(factor)) yield {
      val (x, y) = loc
      val v : Variable2D = DataError2D(x, y)
      (v, factor)
    }
  }.toSet
  val measEdges2D = {
    for((x, y) <- syndromeLocations2D) yield {
      val v : Variable2D = MeasError2D(x, y)
      (v, Factor2D(x, y))
    }
  }.toSet
  val edges2D = dataEdges2D union measEdges2D

  // product with 3D
  /*
  val data_error_labels = {
    for(ij <- dataLocations_2d;
        k <- 0 until num_meas) yield {
      ij match { case (i, j) => DataError(k, i, j) }
    }
  }.toSet
  val syndromeLocations = {
    for(ij <- syndromeLocations_2d;
        k <- 0 until distance) yield {
      ij match { case (i, j) => (k, i, j) }
    }
  }
  val meas_error_labels = {
    syndromeLocations_3d.map(MeasurementError.tupled)
  }
  val factors = {
    syndromeLocations_3d.map(FactorLabel.tupled)
  }
  val variables = {
    val data_error_base = data_error_labels.map[VariableLabel](identity)
    val meas_error_base = meas_error_labels.map[VariableLabel](identity)
    data_error_base union meas_error_base
  }
  */

  /** For a rotated-code check at (x, y), return adjacent integer data qubits. */

/*
  val dataEdges = {
    for(((i, j), (x, y)) <- dataEdges_2d;
        k <- 0 until distance) yield {
      val variable : VariableLabel = DataError(k, i, j)
      val factor = FactorLabel(k, x, y)
      (variable, factor)
    }
  }

  val measEdges: Set[(VariableLabel, FactorLabel)] =
    syndromeLocations_3d.map { case (k, x, y) =>
      val variable : VariableLabel = MeasurementError(k, x, y)
      val factor = FactorLabel(k, x, y)
      (variable, factor)
    }

  val edges: Set[(VariableLabel, FactorLabel)] = {
    val measEdges: Set[(VariableLabel, FactorLabel)] =
      syndromeLocations_3d.map { case (k, x, y) =>
        (MeasurementError(k, x, y): VariableLabel, FactorLabel(k, x, y))
      }

    dataEdges union measEdges
  }
  */

  // Instantiate the BP core over these labels/edges (no IO yet).
  // Assumes a definition like:
  // class BeliefQ[V, F](var_labels: Set[V], factor_labels: Set[F], edges: Set[(V, F)]) extends Component
  //val bp = new BeliefQ[VariableLabel, FactorLabel](variables, factors, edges)
}
