package polynomial

import core.IntegerModding._
import org.scalatest.{FunSuite, Matchers}
import polynomial.AdjoiningOperations._
import polynomial.Predef.X
import PolynomialOps._

class PolynomialRingOverFieldTest extends FunSuite with Matchers {

  implicit val intsMod5 = PrimeField(5)
  implicit val polyRing = intsMod5 f_adjoin X
  def classOf = intToResidueClass(5)

  val one = classOf(1)
  val two = classOf(2)
  val three = classOf(3)
  val four = classOf(4)

  test("The quotient of a constant divided by a constant is a constant") {

    val constant1 = polynomial(three)
    val constant2 = polynomial(two)

    polyRing.quot(constant1, constant2) should be (polynomial(four))
  }

  test("The quotient of zero divided by a constant is a zero") {

    val constant = polynomial(three)
    polyRing.quot(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The quotient of any non-zero polynomial divided by zero should throw an exception") {

    val constant = polynomial(three)
    val p = polynomial(one, two)

    intercept[ArithmeticException](polyRing.quot(constant, polyRing.zero))
    intercept[ArithmeticException](polyRing.quot(p, polyRing.zero))
    intercept[ArithmeticException](polyRing.quot(polyRing.zero, polyRing.zero))
  }

  test("The quotient of a polynomial divided by a polynomial of higher degree should be zero") {

    val p1 = polynomial(one, two)
    val p2 = polynomial(one, two, three)

    polyRing.quot(p1, p2) should be (polyRing.zero)
  }

  test("The quotient of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = polynomial(one, two, four)
    val p2 = polynomial(one, two, three)

    polyRing.quot(p1, p2).degree.toInt should be (0)
  }

  test("The quotient of a polynomial divided by a polynomial of lesser degree should have degree equal the difference") {

    val p1 = polynomial(one)
    val p2 = polynomial(one, two)
    val p3 = polynomial(one, two, three)

    polyRing.quot(p2, p1).degree.toInt should be (1)
    polyRing.quot(p3, p1).degree.toInt should be (2)
    polyRing.quot(p3, p2).degree.toInt should be (1)
  }

  test("The quotient for polynomials should work when they divide evenly") {

    val p1 = polynomial(three)
    val p2 = polynomial(one, two)
    val p3 = polynomial(three, two)
    val p4 = polynomial(two, one, two)

    polyRing.quot(p1 * p2, p1) should be (p2)
    polyRing.quot(p1 * p2, p2) should be (p1)

    polyRing.quot(p2 * p3, p3) should be (p2)
    polyRing.quot(p2 * p3, p2) should be (p3)

    polyRing.quot(p2 * p3 * p4, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4, p2 * p3) should be (p4)
  }

  test("The quotient for polynomials should work when they don't divide evenly") {

    val p1 = polynomial(three)
    val p2 = polynomial(one, two)
    val p3 = polynomial(three, two)
    val p4 = polynomial(two, one, two)

    polyRing.quot(polyRing.plus(p2 * p3, p1), p3) should be (p2)

    polyRing.quot(p2 * p3 * p4 + p1, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4 + p2, p4) should be (p2 * p3)
    polyRing.quot(p2 * p3 * p4 + p3, p4) should be (p2 * p3)

    polyRing.quot(p2 * p3 * p4 + p1, p2 * p3) should be (p4)
    polyRing.quot(p2 * p3 * p4 + p2, p2 * p3) should be (p4)
    polyRing.quot(p2 * p3 * p4 + p3, p2 * p3) should be (p4)
  }

  test("The mod of a constant divided by a constant is zero") {

    val constant1 = polynomial(three)
    val constant2 = polynomial(two)

    polyRing.mod(constant1, constant2) should be (polyRing.zero)
  }

  test("The mod of zero divided by a constant is a zero") {

    val constant = polynomial(three)

    polyRing.mod(polyRing.zero, constant) should be (polyRing.zero)
  }

  test("The mod of any non-zero polynomial divided by zero should throw an exception") {

    val constant = polynomial(three)
    val p = polynomial(one, two)

    intercept[ArithmeticException](polyRing.mod(constant, polyRing.zero))
    intercept[ArithmeticException](polyRing.mod(p, polyRing.zero))
    intercept[ArithmeticException](polyRing.mod(polyRing.zero, polyRing.zero))
  }

  test("The mod of a polynomial divided by a polynomial of higher degree should be itself") {

    val p1 = polynomial(one, two)
    val p2 = polynomial(one, two, three)

    polyRing.mod(p1, p2) should be (p1)
  }

  test("The mod of a polynomial divided by a polynomial of the same degree should have degree 0") {

    val p1 = polynomial(one, two)
    val p2 = polynomial(one, three)

    polyRing.quot(p1, p2).degree.toInt should be (0)
  }

  test("The mod of a polynomial divided by a polynomial of lesser degree should have degree <= the degree of the numerator") {

    val p1 = polynomial(one, two, three)
    val p2 = polynomial(one, three)

    assert(polyRing.quot(p1, p2).degree <= p1.degree)
  }

  test("The mod for polynomials that divide evenly should be zero") {

    val p1 = polynomial(three)
    val p2 = polynomial(one, two)
    val p3 = polynomial(three, two)
    val p4 = polynomial(two, one, two)


    polyRing.mod(p1 * p2, p1) should be (polyRing.zero)
    polyRing.mod(p1 * p2, p2) should be (polyRing.zero)

    polyRing.mod(p2 * p3, p3) should be (polyRing.zero)
    polyRing.mod(p2 * p3, p2) should be (polyRing.zero)

    polyRing.mod(p2 * p3 * p4, p4) should be (polyRing.zero)
    polyRing.mod(p2 * p3 * p4, p2 * p3) should be (polyRing.zero)
  }

  test("The mod for polynomials should work when they don't divide evenly") {

    val p1 = polynomial(three)
    val p2 = polynomial(one, two)
    val p3 = polynomial(three, two)
    val p4 = polynomial(two, one, two)

    polyRing.mod(p2 * p3 + p1, p3) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p1, p4) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p2, p4) should be (p2)
    polyRing.mod(p2 * p3 * p4 + p3, p4) should be (p3)

    polyRing.mod(p2 * p3 * p4 + p1, p2 * p3) should be (p1)
    polyRing.mod(p2 * p3 * p4 + p2, p2 * p3) should be (p2)
    polyRing.mod(p2 * p3 * p4 + p3, p2 * p3) should be (p3)
  }

}
