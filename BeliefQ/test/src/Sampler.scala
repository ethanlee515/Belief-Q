package beliefq
package test

import scala.util.Random

object Sampler {
  val random = new Random()
  def random_message() : BigDecimal = {
    val n = random.nextInt(10)
    val frac = random.nextInt(256)
    return n + frac / BigDecimal(256)
  }
  def random_boolean() : Boolean = {
    return random.nextBoolean()
  }
}
