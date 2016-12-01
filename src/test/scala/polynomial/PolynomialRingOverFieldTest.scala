package polynomial

import core.IntegerModding._
import polynomial.Predef.X
import AdjoiningOperations._
import core.ResidueClass
import org.scalatest.{FunSuite, Matchers}

class PolynomialRingOverFieldTest extends FunSuite with Matchers {

  implicit val intsMod5 = PrimeField(5)
  implicit val polyRing = intsMod5 adjoin X
  implicit def converter = intToResidueClass(5)

  //TODO once more, a classof method would be nice
  def poly(xs: ResidueClass[Int]*) = polyRing.polynomial(xs: _*)

  test("The quotient of a constant divided by a constant is a constant") {

    val constant1 = poly(3)
    val constant2 = poly(2)

    polyRing.quot(constant1, constant2) should be (poly(4))
  }

  test("The quotient of zero divided by a constant is a zero") {

    val constant = poly(3)
    polyRing.quot(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The quotient of any non-zero polynomial divided by zero should throw an exception") {

    val constant = poly(3)
    val p = poly(1, 2)

    intercept[ArithmeticException](polyRing.quot(constant, polyRing.zero))
    intercept[ArithmeticException](polyRing.quot(p, polyRing.zero))
    intercept[ArithmeticException](polyRing.quot(polyRing.zero, polyRing.zero))
  }

  test("The quotient of a polynomial divided by a polynomial of higher degree should be zero") {

    val p1 = poly(1, 2)
    val p2 = poly(1, 2, 3)

    polyRing.quot(p1, p2) should be (polyRing.zero)
  }

  test("The quotient of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = poly(1, 2, 4)
    val p2 = poly(1, 2, 3)

    polyRing.quot(p1, p2).degree.toInt should be (0)
  }

  test("The quotient of a polynomial divided by a polynomial of lesser degree should have degree equal the difference") {

    val p1 = poly(1)
    val p2 = poly(1, 2)
    val p3 = poly(1, 2, 3)

    polyRing.quot(p2, p1).degree.toInt should be (1)
    polyRing.quot(p3, p1).degree.toInt should be (2)
    polyRing.quot(p3, p2).degree.toInt should be (1)
  }

  test("The quotient for polynomials should work when they divide evenly") {

    val p1 = poly(3)
    val p2 = poly(1, 2)
    val p3 = poly(3, 2)
    val p4 = poly(2, 1, 2)

    polyRing.quot(p1 * p2, p1) should be (p2)
    polyRing.quot(p1 * p2, p2) should be (p1)

    polyRing.quot(p2 * p3, p3) should be (p2)
    polyRing.quot(p2 * p3, p2) should be (p3)

    polyRing.quot(p2 * p3 * p4, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4, p2 * p3) should be (p4)
  }

  test("The quotient for polynomials should work when they don't divide evenly") {

    val p1 = polyRing.polynomial(3)
    val p2 = polyRing.polynomial(1, 2)
    val p3 = polyRing.polynomial(3, 2)
    val p4 = polyRing.polynomial(2, 1, 2)

    polyRing.quot(polyRing.plus(p2 * p3, p1), p3) should be (p2)

    polyRing.quot(p2 * p3 * p4 + p1, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4 + p2, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4 + p3, p4) should be (p2 * p3)

    polyRing.quot(p2 * p3 * p4 + p1, p2 * p3) should be (p4)
    polyRing.quot(p2 * p3 * p4 + p2, p2 * p3) should be (p4)
    polyRing.quot(p2 * p3 * p4 + p3, p2 * p3) should be (p4)
  }

  test("The mod of a constant divided by a constant is zero") {

    val constant1 = poly(3)
    val constant2 = poly(2)

    polyRing.mod(constant1, constant2) should be (polyRing.zero)
  }

  test("The mod of zero divided by a constant is a zero") {

    val constant = poly(3)

    polyRing.mod(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The mod of any non-zero polynomial divided by zero should throw an exception") {

    val constant = poly(3)
    val p = poly(1, 2)

    intercept[ArithmeticException](polyRing.mod(constant, polyRing.zero))
    intercept[ArithmeticException](polyRing.mod(p, polyRing.zero))
    intercept[ArithmeticException](polyRing.mod(polyRing.zero, polyRing.zero))
  }

  test("The mod of a polynomial divided by a polynomial of higher degree should be itself") {

    val p1 = poly(1, 2)
    val p2 = poly(1, 2, 3)

    polyRing.mod(p1, p2) should be (p1)
  }

  test("The mod of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = poly(1, 2)
    val p2 = poly(1, 3)

    polyRing.quot(p1, p2).degree.toInt should be (0)
  }

  test("The mod of a polynomial divided by a polynomial of lesser degree should have degree <= the degree of the numerator") {

    val p1 = poly(1, 2, 3)
    val p2 = poly(1, 3)

    assert(polyRing.quot(p1, p2).degree <= polyRing.degree(p1))
  }

  test("The mod for polynomials that divide evenly should be zero") {

    val p1 = poly(3)
    val p2 = poly(1, 2)
    val p3 = poly(3, 2)
    val p4 = poly(2, 1, 2)


    polyRing.mod(p1 * p2, p1) should be (polyRing.zero)
    polyRing.mod(p1 * p2, p2) should be (polyRing.zero)

    polyRing.mod(p2 * p3, p3) should be (polyRing.zero)
    polyRing.mod(p2 * p3, p2) should be (polyRing.zero)

    polyRing.mod(p2 * p3 * p4, p4) should be (polyRing.zero)
    polyRing.mod(p2 * p3 * p4, p2 * p3) should be (polyRing.zero)
  }

  test("The mod for polynomials should work when they don't divide evenly") {

    val p1 = poly(3)
    val p2 = poly(1, 2)
    val p3 = poly(3, 2)
    val p4 = poly(2, 1, 2)

    polyRing.mod(p2 * p3 + p1, p3) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p1, p4) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p2, p4) should be (p2)
    polyRing.mod(p2 * p3 * p4 + p3, p4) should be (p3)

    polyRing.mod(p2 * p3 * p4 + p1, p2 * p3) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p2, p2 * p3) should be (p2)
    polyRing.mod(p2 * p3 * p4 + p3, p2 * p3) should be (p3)
  }

}
