package polynomial

import finitefields.{Converter, IntegersMod, ResidueClass}
import org.scalatest.{FunSuite, Matchers}
import polynomial.Predef.X
import spotcheck.CommutativeRingCheck

class Polynomial1Test extends FunSuite with Matchers {

  implicit def intsMod4 = IntegersMod(4)
  implicit def converter = Converter(intsMod4)
  implicit def polyRing = Polynomial1Ring(X, intsMod4)
  def poly(xs: ResidueClass*) = polyRing.polynomial(xs: _*)
  val zero = intsMod4.zero

  test("Leading zero coefficients are ignored") {

    poly(1, 2, 0, 3) should be (poly(0, 0, 1, 2, 0, 3))
  }

  test("The leading coefficient is zero only for the zero polynomial") {

    val poly1 = poly(1, 2, 3)
    val poly2 = poly(0, 2, 3, 3)

    poly1.leadingCoefficient should be (intsMod4.residueClass(1))
    poly2.leadingCoefficient should be (intsMod4.residueClass(2))
    polyRing.zero.leadingCoefficient should be (zero)
  }

  test("The degree function works as expected") {

    poly(2, 2, 0, 1).degree should be (3)
    poly(0, 2, 2, 0, 1).degree should be (3)

    poly(3, 2, 1).degree should be (2)
    poly(2, 1).degree should be (1)
    poly(3).degree should be (0)
  }

  //TODO: No it isn't, it's negative infinity!
  test("By convention, the degree of the zero polynomial is defined as -1") {

    polyRing.zero.degree should be (-1)
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

    polyRing.negate(poly(1, 3, 3)) should be (poly(3, 1, 1))
  }

  test("The ring has zero and one") {

    polyRing.zero should be (poly(0))
    polyRing.one should be (poly(1))
  }

  test("It is a commutative ring") {

    val degreeZero = (0 to 3) map {poly(_)}
    val degreeOne = degreeZero map {_ * poly(1, 0)}
    val degreeTwo = degreeOne map {_ * poly(1, 0)}
    val degreeThree = degreeTwo map {_ * poly(1, 0)}

    CommutativeRingCheck(degreeZero ++ degreeOne ++ degreeTwo ++ degreeThree) should be (true)
  }
}
