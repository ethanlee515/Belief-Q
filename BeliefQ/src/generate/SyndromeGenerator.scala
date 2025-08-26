package beliefq
package generate

import scala.util.Random

class LogPriorSampler(distance: Int, num_meas: Int) {
  val random = new Random()
  val geo = new SurfaceCodeGeometry(distance, num_meas)
  val results = {
    for(v <- geo.variables) yield {
      val numerator = random.nextInt(16)
      // unrealistically high error rate so we observe *something*
      v match {
        case Variable3D(t, DataError2D(x, y)) => {
          v -> (numerator / BigDecimal(32) - 4)
        }
        case Variable3D(t, MeasError2D(x, y)) => {
          v -> (numerator / BigDecimal(32) - 5)
        }
      }
    }
  }.toMap
}

class SyndromeSampler(log_priors: Map[Variable3D, BigDecimal]) {
  val random = new Random()
  val results = {
    for((v, log_prior) <- log_priors) yield {
      val prior = math.pow(2, log_prior.toDouble)
      println(f"prior = $prior")
      v -> (random.nextDouble() < prior)
    }
  }
  println(results)
}
