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
      SimConfig.compile { new TwoMins3(params, 3) }.doSim { dut =>
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

    test("TwoMins") {
      SimConfig.compile { new TwoMins(params, 35) }.doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        cd.waitSampling()
        val inputs = Seq.tabulate(35)(i => 100 + i)
          .updated(17, 3)
          .updated(34, 5)
        for(i <- inputs.indices) {
          dut.data(i) #= inputs(i)
        }
        cd.waitSampling(dut.delays + 1)
        assert(dut.min1.toBigDecimal == 3)
        assert(dut.id_min1.toInt == 17)
        assert(dut.min2.toBigDecimal == 5)
        assert(dut.id_min2.toInt == 34)
      }
    }

    test("CToV hardware vs golden reference") {
      SimConfig.compile { new CToV(params, 35) }.doSim { dut =>
        dut.inputs.valid #= false
        val cd = dut.clockDomain
        cd.forkStimulus(10)
        cd.assertReset()
        sleep(100)
        cd.deassertReset()
        sleep(100)
        assert(dut.delays == TwoMins.delaysFor(35) + 4)
        for(_ <- 0 until 10) {
          val messages = List.fill(35)(random_message())
          val syndrome = random_boolean()
          val results = reference.CToV.compute(syndrome, messages)
          dut.inputs.valid #= true
          for(i <- 0 until 35) {
            dut.inputs.payload.raw_messages(i) #= message_bits(params, messages(i))
          }
          dut.inputs.payload.syndrome #= syndrome
          cd.waitSampling()
          dut.inputs.valid #= false
          cd.waitSampling(dut.delays)
          assert(dut.output.valid.toBoolean)
          for(i <- 0 until 35) {
            val xi = dut.output.payload(i).toBigDecimal
            assert(xi == results(i))
          }
          cd.waitSampling()
        }
      }
    }
  }
}
