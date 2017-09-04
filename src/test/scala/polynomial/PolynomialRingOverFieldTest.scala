package polynomial

import integer.IntegersModP
import org.scalatest.{FunSuite, Matchers}
import polynomial.Predef.X
import core.EuclideanDomainOps._

class PolynomialRingOverFieldTest extends FunSuite with Matchers {

  implicit val intsMod5 = IntegersModP(5)
  implicit val polyRing = PolynomialRingOverField(intsMod5, X)

  val zero = intsMod5.classOf(0)
  val one = intsMod5.classOf(1)
  val two = intsMod5.classOf(2)
  val three = intsMod5.classOf(3)
  val four = intsMod5.classOf(4)

  test("Addition works") {

    Polynomial(three) + Polynomial(four) should be (Polynomial(two))
    Polynomial(one, three) + Polynomial(three) should be (Polynomial(one, one))
    Polynomial(two, three) + Polynomial(three, one) should be (Polynomial(four))
    Polynomial(three, zero) + polyRing.zero should be (Polynomial(three, zero))
  }

  test("Subtraction works") {

    Polynomial(three) - Polynomial(two) should be (Polynomial(one))
    Polynomial(one, two) - Polynomial(three) should be (Polynomial(one, four))
    Polynomial(two, three) - Polynomial(two, two) should be (Polynomial(one))
    Polynomial(three, zero) - polyRing.zero should be (Polynomial(three, zero))
  }

  test("Multiplication works") {

    Polynomial(three) * Polynomial(two) should be (Polynomial(one))
    Polynomial(one, two) * Polynomial(three) should be (Polynomial(three, one))
    Polynomial(one, three, two) * Polynomial(three, one) should be (Polynomial(three, zero, four, two))
    Polynomial(one, two, three) * polyRing.zero should be (polyRing.zero)
  }

  test("Exponentiation works") {

    Polynomial(one, three) ^ 0 should be (polyRing.one)
    Polynomial(one, three) ^ 1 should be (Polynomial(one, three))
    Polynomial(one, three) ^ 2 should be (Polynomial(one, one, four))
    Polynomial(one, three) ^ 3 should be (Polynomial(one, four, two, two))
  }

  test("Negation works") {

    Polynomial(one, three, three).negate should be (Polynomial(four, two, two))
  }

  test("A polynomial ring over a field has a zero and a one") {

    polyRing.zero should be (Polynomial(zero))
    polyRing.one should be (Polynomial(one))
  }

  test("The quotient of a constant divided by a constant is a constant") {

    val constant1 = Polynomial(three)
    val constant2 = Polynomial(two)

    polyRing.quot(constant1, constant2) should be (Polynomial(four))
  }

  test("The quotient of zero divided by a constant is a zero") {

    val constant = Polynomial(three)
    polyRing.quot(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The quotient of any non-zero polynomial divided by zero should throw an exception") {

    val constant = Polynomial(three)
    val p = Polynomial(one, two)

    intercept[ArithmeticException](polyRing.quot(constant, polyRing.zero))
    intercept[ArithmeticException](polyRing.quot(p, polyRing.zero))
    intercept[ArithmeticException](polyRing.quot(polyRing.zero, polyRing.zero))
  }

  test("The quotient of a polynomial divided by a polynomial of higher degree should be zero") {

    val p1 = Polynomial(one, two)
    val p2 = Polynomial(one, two, three)

    polyRing.quot(p1, p2) should be (polyRing.zero)
  }

  test("The quotient of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = Polynomial(one, two, four)
    val p2 = Polynomial(one, two, three)

    polyRing.quot(p1, p2).degree.toInt should be (0)
  }

  test("The quotient of a polynomial divided by a polynomial of lesser degree should have degree equal the difference") {

    val p1 = Polynomial(one)
    val p2 = Polynomial(one, two)
    val p3 = Polynomial(one, two, three)

    polyRing.quot(p2, p1).degree.toInt should be (1)
    polyRing.quot(p3, p1).degree.toInt should be (2)
    polyRing.quot(p3, p2).degree.toInt should be (1)
  }

  test("The quotient for polynomials should work when they divide evenly") {

    val p1 = Polynomial(three)
    val p2 = Polynomial(one, two)
    val p3 = Polynomial(three, two)
    val p4 = Polynomial(two, one, two)

    polyRing.quot(p1 * p2, p1) should be (p2)
    polyRing.quot(p1 * p2, p2) should be (p1)

    polyRing.quot(p2 * p3, p3) should be (p2)
    polyRing.quot(p2 * p3, p2) should be (p3)

    polyRing.quot(p2 * p3 * p4, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4, p2 * p3) should be (p4)
  }

  test("The quotient for polynomials should work when they don't divide evenly") {

    val p1 = Polynomial(three)
    val p2 = Polynomial(one, two)
    val p3 = Polynomial(three, two)
    val p4 = Polynomial(two, one, two)

    polyRing.quot(polyRing.plus(p2 * p3, p1), p3) should be (p2)

    polyRing.quot(p2 * p3 * p4 + p1, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4 + p2, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4 + p3, p4) should be (p2 * p3)

    polyRing.quot(p2 * p3 * p4 + p1, p2 * p3) should be (p4)
    polyRing.quot(p2 * p3 * p4 + p2, p2 * p3) should be (p4)
    polyRing.quot(p2 * p3 * p4 + p3, p2 * p3) should be (p4)
  }

  test("The mod of a constant divided by a constant is zero") {

    val constant1 = Polynomial(three)
    val constant2 = Polynomial(two)

    polyRing.mod(constant1, constant2) should be (polyRing.zero)
  }

  test("The mod of zero divided by a constant is a zero") {

    val constant = Polynomial(three)

    polyRing.mod(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The mod of any non-zero polynomial divided by zero should throw an exception") {

    val constant = Polynomial(three)
    val p = Polynomial(one, two)

    intercept[ArithmeticException](polyRing.mod(constant, polyRing.zero))
    intercept[ArithmeticException](polyRing.mod(p, polyRing.zero))
    intercept[ArithmeticException](polyRing.mod(polyRing.zero, polyRing.zero))
  }

  test("The mod of a polynomial divided by a polynomial of higher degree should be itself") {

    val p1 = Polynomial(one, two)
    val p2 = Polynomial(one, two, three)

    polyRing.mod(p1, p2) should be (p1)
  }

  test("The mod of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = Polynomial(one, two)
    val p2 = Polynomial(one, three)

    polyRing.quot(p1, p2).degree.toInt should be (0)
  }

  test("The mod of a polynomial divided by a polynomial of lesser degree should have degree <= the degree of the numerator") {

    val p1 = Polynomial(one, two, three)
    val p2 = Polynomial(one, three)

    assert(polyRing.quot(p1, p2).degree <= p1.degree)
  }

  test("The mod for polynomials that divide evenly should be zero") {

    val p1 = Polynomial(three)
    val p2 = Polynomial(one, two)
    val p3 = Polynomial(three, two)
    val p4 = Polynomial(two, one, two)


    polyRing.mod(p1 * p2, p1) should be (polyRing.zero)
    polyRing.mod(p1 * p2, p2) should be (polyRing.zero)

    polyRing.mod(p2 * p3, p3) should be (polyRing.zero)
    polyRing.mod(p2 * p3, p2) should be (polyRing.zero)

    polyRing.mod(p2 * p3 * p4, p4) should be (polyRing.zero)
    polyRing.mod(p2 * p3 * p4, p2 * p3) should be (polyRing.zero)
  }

  test("The mod for polynomials should work when they don't divide evenly") {

    val p1 = Polynomial(three)
    val p2 = Polynomial(one, two)
    val p3 = Polynomial(three, two)
    val p4 = Polynomial(two, one, two)

    polyRing.mod(p2 * p3 + p1, p3) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p1, p4) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p2, p4) should be (p2)
    polyRing.mod(p2 * p3 * p4 + p3, p4) should be (p3)

    polyRing.mod(p2 * p3 * p4 + p1, p2 * p3) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p2, p2 * p3) should be (p2)
    polyRing.mod(p2 * p3 * p4 + p3, p2 * p3) should be (p3)
  }

}
