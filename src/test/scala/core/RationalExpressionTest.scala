package core

import integer.integers
import org.scalatest.{FunSuite, Matchers}

class RationalExpressionTest extends FunSuite with Matchers {

  test("Use == for testing \"algebraic\" equality") {

    RationalExpression(2, 3) == RationalExpression(2, 3) should be (true)
    RationalExpression(2, 3) == RationalExpression(4, 6) should be (true)
    RationalExpression(2, 3) == RationalExpression(3, 2) should be (false)
  }

  test("Use != for testing \"algebraic\" inequality") {

    RationalExpression(2, 3) != RationalExpression(3, 2) should be (true)
    RationalExpression(2, 3) != RationalExpression(4, 6) should be (false)
  }

  //TODO: suppress compiler warning for equality checking on incompatible types.  See https://github.com/dkettlestrings/thunder/issues/46
  test("Equality checks for compatible types") {

    RationalExpression(2, 1) != 2 should be (true)
  }

  test("Use === for testing representational equality") {

    RationalExpression(2, 3) === RationalExpression(2, 3) should be (true)
    RationalExpression(2, 3) === RationalExpression(4, 6) should be (false)
  }

  test("Use !== for testing representational inequality") {

    (RationalExpression(2, 3) !== RationalExpression(4, 6)) should be (true)
    (RationalExpression(2, 3) !== RationalExpression(2, 3)) should be (false)
  }

}
