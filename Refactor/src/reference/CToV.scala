package beliefq
package reference

object CToV {
  def compute(syndrome: Boolean, vToCs: Seq[BigDecimal]) : Seq[BigDecimal] = {
    require(vToCs.length >= 2)
    Seq.tabulate(vToCs.length) {i =>
      var running_min = (if(i == 0) vToCs(1) else vToCs(0)).abs
      var running_sign = if(syndrome) -1 else 1
      for(j <- 0 until vToCs.length) {
        if(i != j) {
          val mj = vToCs(j)
          val a = mj.abs
          if(a < running_min) {
            running_min = a
          }
          running_sign *= (if (mj < 0) -1 else 1)
        }
      }
      val result = running_sign * running_min
//      if(vToCs(i) == 0) 0 else result
      result
    }
  }
}
