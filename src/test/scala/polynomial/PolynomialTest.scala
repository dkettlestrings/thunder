package polynomial

import core.IntegerModding._
import org.scalatest.{FunSuite, Matchers}
import polynomial.Predef.X
import core.{NegativeInfinity, ResidueClass}
import AdjoiningOperations._

class PolynomialTest extends FunSuite with Matchers {

  implicit val intsMod4 = IntegersMod(4)
  implicit val polyRing = intsMod4 adjoin X
  implicit def converter = intToResidueClass(4)
  def poly(xs: ResidueClass[Int]*) = polyRing.polynomial(xs: _*)

  //TODO: again, we need a classOf operation
  val zero = intsMod4.zero
  val one = intsMod4.one
  val two = one + one

  test("Leading zero coefficients are ignored") {

    poly(1, 2, 0, 3) should be (poly(0, 0, 1, 2, 0, 3))
  }

  test("The leading coefficient is zero only for the zero polynomial") {

    val poly1 = poly(1, 2, 3)
    val poly2 = poly(0, 2, 3, 3)

    poly1.leadingCoefficient should be (one)
    poly2.leadingCoefficient should be (two)
    polyRing.zero.leadingCoefficient should be (zero)
  }

  test("The degree function works as expected, but you have to use the extended integers (so you can get negative infinity)") {

    poly(2, 2, 0, 1).degree.toInt should be (3)
    poly(0, 2, 2, 0, 1).degree.toInt should be (3)

    poly(3, 2, 1).degree.toInt should be (2)
    poly(2, 1).degree.toInt should be (1)
    poly(3).degree.toInt should be (0)
  }

  test("By convention, the degree of the zero polynomial is defined as negative infinity") {

    polyRing.zero.degree should be (NegativeInfinity)
  }

  test("Addition works") {

    poly(3) + poly(2) should be (poly(1))
    poly(1, 3) + poly(2) should be (poly(1, 1))
    poly(2, 3) + poly(2, 2) should be (poly(1))
    poly(3, 0) + polyRing.zero should be (poly(3, 0))
  }

  test("Subtraction works") {

    poly(3) - poly(2) should be (poly(1))
    poly(1, 2) - poly(3) should be (poly(1, 3))
    poly(2, 3) - poly(2, 2) should be (poly(1))
    poly(3, 0) - polyRing.zero should be (poly(3, 0))
  }

  test("Multiplication works") {

    poly(3) * poly(2) should be (poly(2))
    poly(1, 2) * poly(3) should be (poly(3, 2))
    poly(1, 3, 2) * poly(3, 1) should be (poly(3, 2, 1, 2))
    poly(1, 2, 3) * polyRing.zero should be (polyRing.zero)
  }

  test("Exponentiation works") {

    poly(1, 3) ^ 0 should be (polyRing.one)
    poly(1, 3) ^ 1 should be (poly(1, 3))
    poly(1, 3) ^ 2 should be (poly(1, 2, 1))
    poly(1, 3) ^ 3 should be (poly(1, 1, 3, 3))
  }

  test("Negation works") {

    poly(1, 3, 3).negate should be (poly(3, 1, 1))
  }

  test("The ring has zero and one") {

    polyRing.zero should be (poly(0))
    polyRing.one should be (poly(1))
  }
}
