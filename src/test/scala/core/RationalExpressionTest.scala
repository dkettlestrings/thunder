package core

import org.scalatest.{FunSuite, Matchers}

class RationalExpressionTest extends FunSuite with Matchers {

  implicit def integers = Integers()

  test("Use == for testing \"algebraic\" equality") {

    RationalExpression(2, 3) == RationalExpression(2, 3) should be (true)
    RationalExpression(2, 3) == RationalExpression(4, 6) should be (true)
    RationalExpression(2, 3) == RationalExpression(3, 2) should be (false)
  }

  test("Use != for testing \"algebraic\" inequality") {

    RationalExpression(2, 3) != RationalExpression(3, 2) should be (true)
    RationalExpression(2, 3) != RationalExpression(4, 6) should be (false)
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
