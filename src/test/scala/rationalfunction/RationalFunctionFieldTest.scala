package rationalfunction

import finitefields.{Converter, PrimeField, ResidueClass}
import org.scalatest.{FunSuite, Matchers}
import polynomial.Polynomial1
import polynomial.Predef.X

class RationalFunctionFieldTest extends FunSuite with Matchers {

  implicit def intsMod5 = PrimeField(5)
  implicit def converter = Converter(intsMod5)
  implicit def rationalFunctions = RationalFunctionField[ResidueClass, PrimeField](X, intsMod5)
  implicit def polyRing = rationalFunctions.polynomialRing

  def poly(xs: ResidueClass*) = rationalFunctions.polynomialRing.polynomial(xs: _*)
  def rational(a: Polynomial1[ResidueClass], b: Polynomial1[ResidueClass]): RationalFunction[ResidueClass, PrimeField] = {
    rationalFunctions.rationalFunction(a, b)
  }

  test("You can't construct a rational function with denominator zero") {

    intercept[ArithmeticException](rational(polyRing.one, polyRing.zero))
  }

  test("Rational functions are equal (===) if they reduce to the same value") {

    val p1 = poly(1)
    val p2 = poly(2)
    val p3 = poly(1, 2)
    val p4 = poly(3, 4, 1)
    val p5 = poly(2, 2, 1, 3)

    rational(p1, p1) === rational(p2, p2) should be (true)
    rational(p3 * p4, p3 * p5) === rational(p4, p5) should be (true)
  }

  test("But the native equals (==) does not respect this") {

    val p1 = poly(1)
    val p2 = poly(2)

    rational(p1, p1) == rational(p2, p2) should be (false)
  }

  test("Addition works") {

    val r1 = rational(poly(3), poly(4))
    val r2 = rational(poly(3), poly(1, 2))
    val r3 = rational(poly(1, 2), poly(3))
    val r4 = rational(poly(1, 2), poly(2, 3))

    r1 + r1 === rational(poly(1), poly(4)) should be (true)
    r1 + r2 === rational(poly(3, 3), poly(4, 3)) should be (true)
    r2 + r3 === rational(poly(1, 4, 3), poly(3, 1)) should be (true)
    r2 + r4 === rational(poly(1, 0, 3), poly(2, 2, 1)) should be (true)
  }

  test("Subtraction works") {

    val r1 = rational(poly(3), poly(4))
    val r2 = rational(poly(3), poly(1, 2))
    val r3 = rational(poly(1, 2), poly(3))
    val r4 = rational(poly(1, 2), poly(2, 3))

    r1 - r1 === rationalFunctions.zero should be (true)
    r1 - r2 === rational(poly(3, 4), poly(4, 3)) should be (true)
    r2 - r3 === rational(poly(4, 1, 0), poly(3, 1)) should be (true)
    r2 - r4 === rational(poly(1, 3, 0), poly(2, 2, 1))
  }

  test("Negation works") {

    val r = rational(poly(3, 2), poly(2, 4))
    r.negate === rational(poly(2, 3), poly(2, 4)) should be (true)
  }

  test("Multiplication works") {

    val r1 = rational(poly(3), poly(4))
    val r2 = rational(poly(3), poly(1, 2))
    val r3 = rational(poly(1, 2), poly(3))
    val r4 = rational(poly(1, 2), poly(2, 3))

    r1 * r1 === rational(poly(4), poly(1)) should be (true)
    r1 * r2 === rational(poly(4), poly(4, 3)) should be (true)
    r2 * r3 === rationalFunctions.one should be (true)
    r2 * r4 === rational(poly(3, 1), poly(2, 2, 1)) should be (true)
  }

  test("Division works") {

    val r1 = rational(poly(3), poly(4))
    val r2 = rational(poly(3), poly(1, 2))
    val r3 = rational(poly(1, 2), poly(3))
    val r4 = rational(poly(1, 2), poly(2, 3))

    r1 / r1 === rationalFunctions.one should be (true)
    r1 / r2 === rational(poly(3, 1), poly(2)) should be (true)
    r2 / r3 === rational(poly(4), poly(1, 4, 4)) should be (true)
    r2 / r4 === rational(poly(1, 4), poly(1, 4, 4)) should be (true)
  }

  test("Inversion works") {
    val r = rational(poly(3, 2), poly(2, 4))
    r.inv === rational(poly(2, 4), poly(3, 2))
  }

  test("Inverting or dividing by zero throws an exception") {

    intercept[ArithmeticException](rationalFunctions.zero.inv)
    intercept[ArithmeticException](rationalFunctions.one / rationalFunctions.zero)
  }

  test("Exponentiation works") {

    val r = rational(poly(3, 2), poly(2, 4))

    (r ^ 0) === rationalFunctions.one should be (true)
    (r ^ 1) === r should be (true)
    (r ^ 2) === rational(poly(4, 2, 4), poly(4, 1, 1)) should be (true)
  }

  //TODO: Remove this test if non accepts the default quot and mod implementations
  test("A rational function field has the Euclidean ring operations, but they're pointless") {

    rationalFunctions.quot(rational(poly(1, 2), poly(3, 4)), rational(poly(2, 1), poly(3, 1))) === rationalFunctions.one should be (true)
    rationalFunctions.quot(rationalFunctions.zero, rational(poly(2, 1), poly(3, 1))) === rationalFunctions.zero should be (true)
    intercept[ArithmeticException](rationalFunctions.quot(rationalFunctions.one, rationalFunctions.zero))

    rationalFunctions.mod(rational(poly(1, 2), poly(3, 4)), rational(poly(2, 1), poly(3, 1))) === rationalFunctions.zero should be (true)
    intercept[ArithmeticException](rationalFunctions.mod(rationalFunctions.one, rationalFunctions.zero))
  }
}

