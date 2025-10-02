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
    val params = new BeliefQParams(8, 8)

    test("CToVRef") {
      val inputs = List(
        BigDecimal("1.11"),
        BigDecimal("-2.2"),
        BigDecimal("33.3"),
        BigDecimal("-4.4"),
        BigDecimal("0.55"),
        BigDecimal("-6.6"))
      val results = reference.CToV.compute(true, inputs)
//      println(f"VToC results = $results")
    }

    test("TwoMins3") {
      SimConfig.compile { new TwoMins3(params) }.doSim { dut =>
        dut.data(0) #= 5
        dut.ids(0) #= 4
        dut.data(1) #= 2
        dut.ids(1) #= 6
        dut.data(2) #= 7
        dut.ids(2) #= 3
        sleep(1)
        assert(dut.min1.toBigDecimal == 2)
        assert(dut.id_min1.toInt == 6)
        assert(dut.min2.toBigDecimal == 5)
        assert(dut.id_min2.toInt == 4)
      }
    }

    test("TwoMins6") {
      SimConfig.compile { new TwoMins6(params) }.doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        cd.waitSampling()
        dut.data(0) #= 11
        dut.data(1) #= 8
        dut.data(2) #= 4
        dut.data(3) #= 2
        dut.data(4) #= 9
        dut.data(5) #= 5
        cd.waitSampling()
        cd.waitSampling()
        assert(dut.min1.toBigDecimal == 2)
        assert(dut.id_min1.toInt == 3)
        assert(dut.min2.toBigDecimal == 4)
        assert(dut.id_min2.toInt == 2)
      }
    }

    test("TwoMins") {
      SimConfig.compile { new TwoMins(params, 4) }.doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        cd.waitSampling()
        dut.data(0) #= 11
        dut.data(1) #= 3
        dut.data(2) #= 5
        dut.data(3) #= 12
        cd.waitSampling()
        cd.waitSampling()
        assert(dut.min1.toBigDecimal == 3)
        assert(dut.id_min1.toInt == 1)
        assert(dut.min2.toBigDecimal == 5)
        assert(dut.id_min2.toInt == 2)
      }
    }

    test("CToV hardware vs golden reference") {
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
          val results = reference.CToV.compute(syndrome, messages)
          dut.inputs.valid #= true
          for(i <- 0 until 5) {
            dut.inputs.payload.messages(i) #= messages(i)
          }
          dut.inputs.payload.syndrome #= syndrome
          cd.waitSampling()
          dut.inputs.valid #= false
          cd.waitSampling(dut.delays)
          assert(dut.output.valid.toBoolean)
          for(i <- 0 until 5) {
            val xi = dut.output.payload(i).toBigDecimal
            assert(xi == results(i))
          }
          cd.waitSampling()
        }
      }
    }
  }
}
