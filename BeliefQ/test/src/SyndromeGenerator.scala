package beliefq
package test

import scala.util.Random

class LogPriorSampler(distance: Int, num_meas: Int) {
  val random = new Random()
  val geo = new SurfaceCodeGeometry(distance, num_meas)
  val results = {
    for(v <- geo.variables) yield {
      val numerator = random.nextInt(16)
      v match {
        case Variable3D(t, DataError2D(x, y)) => {
          //v -> (numerator / BigDecimal(32) - 4)
          v -> (4 - numerator / BigDecimal(32))
        }
        case Variable3D(t, MeasError2D(x, y)) => {
          //v -> (numerator / BigDecimal(32) - 5)
          v -> (5 - numerator / BigDecimal(32))
        }
      }
    }
  }.toMap
}

class SyndromeSampler[V, F](log_priors: Map[V, BigDecimal], geometry: TannerGraphGeometry[V, F]) {
  val random = new Random()
  val errors = {
    for((v, log_prior) <- log_priors) yield {
      val prior = math.pow(2, -log_prior.toDouble)
      v -> (random.nextDouble() < prior)
    }
  }
  val syndromes = {
    for(f <- geometry.chk_labels) yield {
      f -> (for(v <- geometry.get_neighboring_variables(f)) yield {
        errors(v)
      }).reduce(_ ^ _)
    }
  }.toMap
}
