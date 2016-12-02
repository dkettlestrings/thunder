package polynomial

import core.IntegerModding._
import core.NegativeInfinity
import org.scalatest.{FunSuite, Matchers}
import polynomial.AdjoiningOperations._
import polynomial.Predef.X
import PolynomialOps._

class PolynomialRingTest extends FunSuite with Matchers {

  implicit val intsMod4 = IntegersMod(4)
  implicit val polyRing = intsMod4 cr_adjoin_ply X
  def classOf = intToResidueClass(4)

  //TODO: again, we need a classOf operation
  val zero = classOf(0)
  val one = classOf(1)
  val two = classOf(2)
  val three = classOf(3)

  test("Leading zero coefficients are ignored") {

    polynomial(one, two, zero, three) should be (polynomial(zero, zero, one, two, zero, three))
  }

  test("The leading coefficient is zero only for the zero polynomial") {

    val poly1 = polynomial(one, two, three)
    val poly2 = polynomial(zero, two, three, three)

    poly1.leadingCoefficient should be (one)
    poly2.leadingCoefficient should be (two)
    polyRing.zero.leadingCoefficient should be (zero)
  }

  test("The degree function works as expected, but you have to use the extended integers (so you can get negative infinity)") {

    polynomial(two, two, zero, one).degree.toInt should be (3)
    polynomial(zero, two, two, zero, one).degree.toInt should be (3)

    polynomial(three, two, one).degree.toInt should be (2)
    polynomial(two, one).degree.toInt should be (1)
    polynomial(three).degree.toInt should be (0)
  }

  test("By convention, the degree of the zero polynomial is defined as negative infinity") {

    polyRing.zero.degree should be (NegativeInfinity)
  }

  test("Addition works") {

    polynomial(three) + polynomial(two) should be (polynomial(one))
    polynomial(one, three) + polynomial(two) should be (polynomial(one, one))
    polynomial(two, three) + polynomial(two, two) should be (polynomial(one))
    polynomial(three, zero) + polyRing.zero should be (polynomial(three, zero))
  }

  test("Subtraction works") {

    polynomial(three) - polynomial(two) should be (polynomial(one))
    polynomial(one, two) - polynomial(three) should be (polynomial(one, three))
    polynomial(two, three) - polynomial(two, two) should be (polynomial(one))
    polynomial(three, zero) - polyRing.zero should be (polynomial(three, zero))
  }

  test("Multiplication works") {

    polynomial(three) * polynomial(two) should be (polynomial(two))
    polynomial(one, two) * polynomial(three) should be (polynomial(three, two))
    polynomial(one, three, two) * polynomial(three, one) should be (polynomial(three, two, one, two))
    polynomial(one, two, three) * polyRing.zero should be (polyRing.zero)
  }

  test("Exponentiation works") {

    polynomial(one, three) ^ 0 should be (polyRing.one)
    polynomial(one, three) ^ 1 should be (polynomial(one, three))
    polynomial(one, three) ^ 2 should be (polynomial(one, two, one))
    polynomial(one, three) ^ 3 should be (polynomial(one, one, three, three))
  }

  test("Negation works") {

    polynomial(one, three, three).negate should be (polynomial(three, one, one))
  }

  test("The ring has zero and one") {

    polyRing.zero should be (polynomial(zero))
    polyRing.one should be (polynomial(one))
  }
}
