package polynomial

import finitefields.{Converter, PrimeField}
import Predef.X
import org.scalatest.{FunSuite, Matchers}

class PolynomialDividerTest extends FunSuite with Matchers {

  val intsMod5 = PrimeField(5)
  implicit val field = intsMod5
  implicit def converter = Converter(intsMod5)
  val polyRing = Polynomial1Ring(X, intsMod5)
  implicit val ring = polyRing

  test("The quotient of a constant divided by a constant is a constant") {

    val constant1 = polyRing.polynomial(3)
    val constant2 = polyRing.polynomial(2)

    PolynomialDivider.quot(constant1, constant2) should be (polyRing.polynomial(4))
  }

  test("The quotient of zero divided by a constant is a zero") {

    val constant = polyRing.polynomial(3)
    PolynomialDivider.quot(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The quotient of any non-zero polynomial divided by zero should throw an exception") {

    val constant = polyRing.polynomial(3)
    val poly = polyRing.polynomial(1, 2)

    intercept[ArithmeticException](PolynomialDivider.quot(constant, polyRing.zero))
    intercept[ArithmeticException](PolynomialDivider.quot(poly, polyRing.zero))
    intercept[ArithmeticException](PolynomialDivider.quot(polyRing.zero, polyRing.zero))
  }

  test("The quotient of a polynomial divided by a polynomial of higher degree should be zero") {

    val p1 = polyRing.polynomial(1, 2)
    val p2 = polyRing.polynomial(1, 2, 3)

    PolynomialDivider.quot(p1, p2) should be (polyRing.zero)
  }

  test("The quotient of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = polyRing.polynomial(1, 2, 4)
    val p2 = polyRing.polynomial(1, 2, 3)

    polyRing.degree(PolynomialDivider.quot(p1, p2)) should be (0)
  }

  test("The quotient of a polynomial divided by a polynomial of lesser degree should have degree equal the difference") {

    val p1 = polyRing.polynomial(1)
    val p2 = polyRing.polynomial(1, 2)
    val p3 = polyRing.polynomial(1, 2, 3)

    polyRing.degree(PolynomialDivider.quot(p2, p1)) should be (1)
    polyRing.degree(PolynomialDivider.quot(p3, p1)) should be (2)
    polyRing.degree(PolynomialDivider.quot(p3, p2)) should be (1)
  }

  test("The quotient for polynomials should work when they divide evenly") {

    val p1 = polyRing.polynomial(3)
    val p2 = polyRing.polynomial(1, 2)
    val p3 = polyRing.polynomial(3, 2)
    val p4 = polyRing.polynomial(2, 1, 2)

    val p12 = polyRing.times(p1, p2)
    val p23 = polyRing.times(p2, p3)
    val p234 = polyRing.times(p2, polyRing.times(p3, p4))

    PolynomialDivider.quot(p12, p1) should be (p2)
    PolynomialDivider.quot(p12, p2) should be (p1)

    PolynomialDivider.quot(p23, p3) should be (p2)
    PolynomialDivider.quot(p23, p2) should be (p3)

    PolynomialDivider.quot(p234, p4) should be (p23)
    PolynomialDivider.quot(p234, p23) should be (p4)
  }

  test("The quotient for polynomials should work when they don't divide evenly") {

    val p1 = polyRing.polynomial(3)
    val p2 = polyRing.polynomial(1, 2)
    val p3 = polyRing.polynomial(3, 2)
    val p4 = polyRing.polynomial(2, 1, 2)

    val p12 = polyRing.times(p1, p2)
    val p23 = polyRing.times(p2, p3)
    val p234 = polyRing.times(p2, polyRing.times(p3, p4))

    PolynomialDivider.quot(polyRing.plus(p23, p1), p3) should be (p2)

    PolynomialDivider.quot(polyRing.plus(p234, p1), p4) should be (p23)
    PolynomialDivider.quot(polyRing.plus(p234, p2), p4) should be (p23)
    PolynomialDivider.quot(polyRing.plus(p234, p3), p4) should be (p23)

    PolynomialDivider.quot(polyRing.plus(p234, p1), p23) should be (p4)
    PolynomialDivider.quot(polyRing.plus(p234, p2), p23) should be (p4)
    PolynomialDivider.quot(polyRing.plus(p234, p3), p23) should be (p4)
  }

  test("The mod of a constant divided by a constant is zero") {

    val constant1 = polyRing.polynomial(3)
    val constant2 = polyRing.polynomial(2)

    PolynomialDivider.mod(constant1, constant2) should be (polyRing.zero)
  }

  test("The mod of zero divided by a constant is a zero") {

    val constant = polyRing.polynomial(3)

    PolynomialDivider.mod(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The mod of any non-zero polynomial divided by zero should throw an exception") {

    val constant = polyRing.polynomial(3)
    val poly = polyRing.polynomial(1, 2)

    intercept[ArithmeticException](PolynomialDivider.mod(constant, polyRing.zero))
    intercept[ArithmeticException](PolynomialDivider.mod(poly, polyRing.zero))
    intercept[ArithmeticException](PolynomialDivider.mod(polyRing.zero, polyRing.zero))
  }

  test("The mod of a polynomial divided by a polynomial of higher degree should be itself") {

    val p1 = polyRing.polynomial(1, 2)
    val p2 = polyRing.polynomial(1, 2, 3)

    PolynomialDivider.mod(p1, p2) should be (p1)
  }

  test("The mod of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = polyRing.polynomial(1, 2)
    val p2 = polyRing.polynomial(1, 3)

    polyRing.degree(PolynomialDivider.quot(p1, p2)) should be (0)
  }

  test("The mod of a polynomial divided by a polynomial of lesser degree should have degree <= the degree of the numerator") {

    val p1 = polyRing.polynomial(1, 2, 3)
    val p2 = polyRing.polynomial(1, 3)

    assert(polyRing.degree(PolynomialDivider.quot(p1, p2)) <= polyRing.degree(p1))
  }

  test("The mod for polynomials that divide evenly should be zero") {

    val p1 = polyRing.polynomial(3)
    val p2 = polyRing.polynomial(1, 2)
    val p3 = polyRing.polynomial(3, 2)
    val p4 = polyRing.polynomial(2, 1, 2)

    val p12 = polyRing.times(p1, p2)
    val p23 = polyRing.times(p2, p3)
    val p234 = polyRing.times(p2, polyRing.times(p3, p4))

    PolynomialDivider.mod(p12, p1) should be (polyRing.zero)
    PolynomialDivider.mod(p12, p2) should be (polyRing.zero)

    PolynomialDivider.mod(p23, p3) should be (polyRing.zero)
    PolynomialDivider.mod(p23, p2) should be (polyRing.zero)

    PolynomialDivider.mod(p234, p4) should be (polyRing.zero)
    PolynomialDivider.mod(p234, p23) should be (polyRing.zero)
  }

  test("The mod for polynomials should work when they don't divide evenly") {

    val p1 = polyRing.polynomial(3)
    val p2 = polyRing.polynomial(1, 2)
    val p3 = polyRing.polynomial(3, 2)
    val p4 = polyRing.polynomial(2, 1, 2)

    val p12 = polyRing.times(p1, p2)
    val p23 = polyRing.times(p2, p3)
    val p234 = polyRing.times(p2, polyRing.times(p3, p4))

    PolynomialDivider.mod(polyRing.plus(p23, p1), p3) should be (p1)
    PolynomialDivider.mod(polyRing.plus(p234, p1), p4) should be (p1)
    PolynomialDivider.mod(polyRing.plus(p234, p2), p4) should be (p2)
    PolynomialDivider.mod(polyRing.plus(p234, p3), p4) should be (p3)

    PolynomialDivider.mod(polyRing.plus(p234, p1), p23) should be (p1)
    PolynomialDivider.mod(polyRing.plus(p234, p2), p23) should be (p2)
    PolynomialDivider.mod(polyRing.plus(p234, p3), p23) should be (p3)
  }

}
