package beliefq
package test

import scala.util.Random
import reference._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.relay._

object TestCToV extends TestSuite {
  val random = new Random(0)

  def random_message() : BigDecimal = {
    val n = random.nextInt(10)
    val frac = random.nextInt(16)
    n + frac / BigDecimal(16)
  }

  def random_boolean() : Boolean = {
    random.nextBoolean()
  }

  def message_bits(params: RelayParams, message: BigDecimal) : BigInt = {
    val scale = BigDecimal(BigInt(1) << params.message_fractional_precision)
    val scaled = (message * scale).toBigInt
    if(scaled < 0) {
      (BigInt(1) << params.var_msg_len) + scaled
    } else {
      scaled
    }
  }

  def tests = Tests {
    val params = new RelayParams()

    test("CToVRef") {
      val inputs = List(
        BigDecimal("1.11"),
        BigDecimal("-2.2"),
        BigDecimal("33.3"),
        BigDecimal("-4.4"),
        BigDecimal("0.55"),
        BigDecimal("-6.6"))
      reference.CToV.compute(true, inputs)
    }

    test("TwoMins3") {
      SimConfig.compile { new TwoMins3(params) }.doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        cd.waitSampling()
        dut.data(0) #= 5
        dut.ids(0) #= 4
        dut.data(1) #= 2
        dut.ids(1) #= 6
        dut.data(2) #= 7
        dut.ids(2) #= 3
        cd.waitSampling()
        cd.waitSampling()
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
        cd.waitSampling()
        cd.waitSampling()
        assert(dut.min1.toBigDecimal == 2)
        assert(dut.id_min1.toInt == 3)
        assert(dut.min2.toBigDecimal == 4)
        assert(dut.id_min2.toInt == 2)
      }
    }

    test("TwoMins9") {
      SimConfig.compile { new TwoMins9(params) }.doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        cd.waitSampling()
        val inputs = Seq(11, 8, 4, 2, 9, 5, 7, 1, 6)
        for(i <- 0 until 9) {
          dut.data(i) #= inputs(i)
        }
        cd.waitSampling()
        cd.waitSampling()
        cd.waitSampling()
        cd.waitSampling()
        assert(dut.min1.toBigDecimal == 1)
        assert(dut.id_min1.toInt == 7)
        assert(dut.min2.toBigDecimal == 2)
        assert(dut.id_min2.toInt == 3)
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
        cd.waitSampling()
        cd.waitSampling()
        assert(dut.min1.toBigDecimal == 3)
        assert(dut.id_min1.toInt == 1)
        assert(dut.min2.toBigDecimal == 5)
        assert(dut.id_min2.toInt == 2)
      }
    }

    test("CToV hardware vs golden reference") {
      SimConfig.compile { new CToV(params, 9) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        for(_ <- 0 until 10) {
          val messages = List.fill(9)(random_message())
          val syndrome = random_boolean()
          val results = reference.CToV.compute(syndrome, messages)
          dut.inputs.valid #= true
          for(i <- 0 until 9) {
            dut.inputs.payload.raw_messages(i) #= message_bits(params, messages(i))
          }
          dut.inputs.payload.syndrome #= syndrome
          cd.waitSampling()
          dut.inputs.valid #= false
          cd.waitSampling(dut.delays)
          assert(dut.output.valid.toBoolean)
          for(i <- 0 until 9) {
            val xi = dut.output.payload(i).toBigDecimal
            assert(xi == results(i))
          }
          cd.waitSampling()
        }
      }
    }
  }
}
