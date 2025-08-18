package beliefq
package reference

object VToCReference {
  def compute(prior: BigDecimal, cToVs: Seq[BigDecimal]) : Seq[BigDecimal] = {
    val s = prior + cToVs.reduce(_ + _)
    return cToVs.map(s - _)
  }
}
