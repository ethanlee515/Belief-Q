package beliefq
package test

import reference._
import utest._
import utest.assert
import spinal.core._
import spinal.core.sim._
import spinal.lib._

object TestCToV extends TestSuite {
  import Sampler._

  def tests = Tests {
    test("CToVRef") {
      val inputs = List(
        BigDecimal("1.11"),
        BigDecimal("-2.2"),
        BigDecimal("33.3"),
        BigDecimal("-4.4"),
        BigDecimal("0.55"),
        BigDecimal("-6.6"))
      val results = CToVReference.compute(true, inputs)
//      println(f"VToC results = $results")
    }

    test("CToV hardware vs golden reference") {
      val params = new BeliefQParams()
      SimConfig.compile { new CToV(params, 5) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        for(shots <- 0 until 10) {
          val messages = List.fill(5)(random_message)
          val syndrome = random_boolean()
          val results = CToVReference.compute(syndrome, messages)
          assert(!cd.waitSamplingWhere(1000) { dut.inputs.ready.toBoolean })
          dut.inputs.valid #= true
          for(i <- 0 until 5) {
            dut.inputs.payload.messages(i) #= messages(i)
          }
          dut.inputs.payload.syndrome #= syndrome
          cd.waitSampling()
          dut.inputs.valid #= false
          assert(!cd.waitSamplingWhere(1000) { dut.output.valid.toBoolean })
          for(i <- 0 until 5) {
            val xi = dut.output.payload(i).toBigDecimal
            assert(xi == results(i))
          }
        }
      }
    }

  }

}
