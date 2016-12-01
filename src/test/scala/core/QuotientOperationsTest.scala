package core

import org.scalatest.{FunSuite, Matchers}
import QuotientOperations._

class QuotientOperationsTest extends FunSuite with Matchers {

  implicit val integers = Integers()
  implicit val rationals = integers quotientField

  val zero = rationals.zero
  val one = rationals.one

  val half = RationalExpression(1, 2)

  test("It has a zero and a one") {

    zero == RationalExpression(0, 1) should be (true)
    one == RationalExpression(1, 1) should be (true)
  }


  test("Addition works as expected") {

    half + zero == half should be (true)
    half + half == one should be (true)
    half + one == RationalExpression(3, 2) should be (true)
    half + one == RationalExpression(6, 4) should be (true)
  }

  test("Negation works as expected") {

    zero.negate == zero should be (true)
    one.negate == RationalExpression(-1, 1) should be (true)
    RationalExpression(3, 2).negate == RationalExpression(-3, 2) should be (true)
  }

  test("Subtraction works as expected") {

    half - zero == half should be (true)
    half - half == zero should be (true)
    RationalExpression(3, 2) - half == one should be (true)
  }

  test("Exponentiation works as expected") {

    (zero ^ 0) == one should be (true) //TODO: is this the behavior we want?
    (zero ^ 8) == zero should be (true)
    (one ^ 5) == one should be (true)
    (half ^ 3) == RationalExpression(1, 8) should be (true)
  }

  test("Multiplication works as expected") {

    zero * one == zero should be (true)
    half * one == half should be (true)
    half * half == RationalExpression(1, 4) should be (true)
    RationalExpression(3, 5) * RationalExpression(10, 7) == RationalExpression(6, 7) should be (true)
  }

  test("Division works as expected") {

    intercept[ArithmeticException](one / zero)
    zero / one == zero should be (true)
    half / half == one should be (true)
    half / one == half should be (true)
    RationalExpression(12, 7) / RationalExpression(6, 5) == RationalExpression(10, 7) should be (true)
  }

}
