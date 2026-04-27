package framework
package test

import utest._
import utest.assert

object TestHello extends TestSuite {
  def tests = Tests {
    test("Hello") {
      assert(true)
    }
  }
}
