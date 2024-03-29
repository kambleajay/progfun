package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Main.balance

  test("'(if (zero? x) max (/ 1 x))' is balanced.") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("'I told him ...' is balanced.") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("':-)' is unbalanced.") {
    assert(!balance(":-)".toList))
  }

  test("Counting is not enough.") {
    assert(!balance("())(".toList))
  }
  
  test("empty string passes") {
    assert(balance("".toList))
  }
  
  test("()()()") {
    assert(balance("()()()".toList))
  }
  
  test("((()))") {
    assert(balance("((()))".toList))
  }
  
  test("only opening") {
    assert(!balance("(".toList))
  }
}
