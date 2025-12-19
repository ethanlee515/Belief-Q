package beliefq
package test

import scala.util.Random
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import beliefq.relay.Lfsr64

object SimRng extends App {
  val params = new BeliefQParams()
  val random = new Random()
  val seed = BigInt(64, random)
  SimConfig.compile { new Lfsr64(params, seed) }.doSim { dut =>
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.assertReset()
    sleep(100)
    cd.deassertReset()
    sleep(100)
    println(dut.rng_norm1.toBigDecimal)
    cd.waitSampling(30)
    println(dut.rng_norm1.toBigDecimal)
    cd.waitSampling(30)
    println(dut.rng_norm1.toBigDecimal)
    cd.waitSampling(30)
    println(dut.rng_norm1.toBigDecimal)
    cd.waitSampling(30)
    println(dut.rng_norm1.toBigDecimal)
    cd.waitSampling(30)
    println(dut.rng_norm1.toBigDecimal)
    cd.waitSampling(30)
    println(dut.rng_norm1.toBigDecimal)
    cd.waitSampling(30)
  }
}
