package beliefq
package test
import reference._
import utest._
import utest.assert

object TestCToV extends TestSuite {
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
      println(f"VToC results = $results")

    }
  }

}
