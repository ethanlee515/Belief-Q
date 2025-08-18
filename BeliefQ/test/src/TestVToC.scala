package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import reference._
import spire.math.extras.{FixedPoint, FixedScale}
import scala.util.Random

object TestVToC extends TestSuite {
  val random = new Random()

  def random_message : BigDecimal = {
    val n = random.nextInt(10)
    val frac = random.nextInt(256)
    return n + frac / BigDecimal(256)
  }

  def tests = Tests {
    val params = new BeliefQParams
    test("test v to c message") {
      SimConfig.compile { new VToC(params, 5) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        for(a <- 0 until 10) {
          val prior = random_message
          val messages = List(random_message, random_message, random_message, random_message, random_message)
          val res = VToCReference.compute(prior, messages)
          assert(!cd.waitSamplingWhere(1000) { dut.inputs.ready.toBoolean })
          dut.inputs.valid #= true
          dut.inputs.payload.prior #= prior
          for(i <- 0 until 5) {
            dut.inputs.payload.messages(i) #= messages(i)
          }
          cd.waitSampling()
          dut.inputs.valid #= false
          assert(!cd.waitSamplingWhere(1000) { dut.output.valid.toBoolean })
          for(i <- 0 until 5) {
            val xi = dut.output.payload(i).toBigDecimal
            assert(xi == res(i))
          }
        }
      }
    }
  }
}
