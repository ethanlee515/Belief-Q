package beliefq
package generate

import scala.util.Random

class LogPriorSampler(distance: Int, num_meas: Int) {
  val random = new Random()
  val geo = new SurfaceCodeGeometry(distance, num_meas)
  val log_priors = {
    for(v <- geo.variables) yield {
      val numerator = random.nextInt(16)
      v match {
        case Variable3D(t, DataError2D(x, y)) => {
          // ~1%; -6.5 to -7
          v -> (numerator / BigDecimal(32) - 7)
        }
        case Variable3D(t, MeasError2D(x, y)) => {
          // ~0.5%; -7.5 to -8
          v -> (numerator / BigDecimal(32) - 8)
        }
      }
    }
  }.toMap
}

class SyndromeSampler(log_priors: Map[Variable3D, BigDecimal]) {
  val random = new Random()
  val errors = {
    for((v, log_prior) <- log_priors) yield {
      val prior = math.pow(2, log_prior.toDouble)
      println(f"prior = $prior")
      // artificially 10x the "reasonable" error rate so we observe *something*
      v -> (random.nextDouble() < 10 * prior)
    }
  }
  println(errors)
}
