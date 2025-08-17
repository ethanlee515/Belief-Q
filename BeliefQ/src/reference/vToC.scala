package beliefq
package reference

import spire.math.extras.{FixedPoint, FixedScale}
import spire.math._
import spire.implicits._

object VToCReference {
  def compute(prior: FixedPoint, cToVs: Seq[FixedPoint]) : Seq[FixedPoint] = {
    val s = prior + cToVs.reduce(_ + _)
    return cToVs.map(s - _)
  }
}
