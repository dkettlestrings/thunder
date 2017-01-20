package polynomial

import core.IntegersModN
import polynomial.Predef.X
import org.scalatest.{FunSuite, Matchers}

class PolynomialTest extends FunSuite with Matchers {

  implicit val intsMod4 = IntegersModN(4)

  val zero = intsMod4.classOf(0)
  val one = intsMod4.classOf(1)
  val two = intsMod4.classOf(2)
  val three = intsMod4.classOf(3)

  test("Leading zero coefficients are ignored") {

    Polynomial(one, two, zero, three) should be (Polynomial(zero, zero, one, two, zero, three))
  }

  test("The leading coefficient is zero only for the polynomial of all zero coefficients") {

    val zeros = Polynomial(zero, zero)
    val poly1 = Polynomial(one, two, three)
    val poly2 = Polynomial(zero, two, three, three)

    zeros.leadingCoefficient should be (zero)
    poly1.leadingCoefficient should be (one)
    poly2.leadingCoefficient should be (two)
  }

  test("By convention, the polynomial with only zero coefficients has degree negative infinity") {

    Polynomial(zero).degree should be (NegativeInfinity)
    Polynomial(zero, zero).degree should be (NegativeInfinity)
  }

  test("The degree function works as expected, but you have to use the extended integers (since you can get negative infinity)") {

    Polynomial(two, two, zero, one).degree.toInt should be (3)
    Polynomial(zero, two, two, zero, one).degree.toInt should be (3)

    Polynomial(three, two, one).degree.toInt should be (2)
    Polynomial(two, one).degree.toInt should be (1)
    Polynomial(three).degree.toInt should be (0)
  }

  test("The string representation follows mathematical conventions") {

    Polynomial(zero).toString should be ("[0]_4")
    Polynomial(one).toString should be ("[1]_4")
    Polynomial(one, zero).toString should be ("[1]_4X")
    Polynomial(two, zero, one).toString should be ("[2]_4X^2 + [1]_4")
    Polynomial(three, two, one, two).toString should be ("[3]_4X^3 + [2]_4X^2 + [1]_4X + [2]_4")
  }

  test("Polynomials with the same formal parameter are equal when you would expect") {

    Polynomial(zero) == Polynomial(zero, zero) should be (true)
    Polynomial(zero, one, two) == Polynomial(one, two) should be (true)
    Polynomial(one, two) != Polynomial(two, one) should be (true)
  }

  //TODO: Suppress type check warning on equals.  See https://github.com/dkettlestrings/thunder/issues/46
  test("Polynomial equality checks for type") {

    Polynomial(two) == 2 should be (false)
  }
}
