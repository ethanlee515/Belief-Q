package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import reference._

object TestVToC extends TestSuite {
  import Sampler._
  def tests = Tests {
    val params = new BeliefQParams(8, 8)
    test("test v to c message") {
      SimConfig.compile { new VToC(params, 5) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        cd.waitSampling()
        for(a <- 0 until 10) {
          val prior = random_message
          val messages = List.fill(5)(random_message)
          val res = reference.VToC.compute(prior, messages)
          dut.inputs.valid #= true
          dut.inputs.payload.prior #= prior
          for(i <- 0 until 5) {
            dut.inputs.payload.messages(i) #= messages(i)
          }
          cd.waitSampling()
          dut.inputs.valid #= false
          cd.waitSampling(dut.delays)
          assert(dut.output.valid.toBoolean)
          for(i <- 0 until 5) {
            val xi = dut.output.payload(i).toBigDecimal
            assert(xi == res(i))
          }
          cd.waitSampling()
        }
      }
    }
  }
}
