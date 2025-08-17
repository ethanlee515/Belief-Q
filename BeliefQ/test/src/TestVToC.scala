package beliefq
package test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import utest._
import utest.assert
import reference._
import spire.math.extras.{FixedPoint, FixedScale}

object TestVToC extends TestSuite {
  def tests = Tests {
    val params = new BeliefQParams
    implicit val scale = FixedScale(1024)
    test("test v to c message") {
      val res = VToCReference.compute(FixedPoint("10"), List(FixedPoint("1"), FixedPoint("2"), FixedPoint("3")))
      assert(res(0).toBigDecimal == 15)
      assert(res(1).toBigDecimal == 14)
      assert(res(2).toBigDecimal == 13)
    }
  }
}
