package polynomial

import core.IntegerModding._
import org.scalatest.{FunSuite, Matchers}
import polynomial.AdjoiningOperations._
import polynomial.Predef.X

class PolynomialRingTest extends FunSuite with Matchers {

  implicit val intsMod4 = IntegersMod(4)
  implicit val polyRing = intsMod4 r_adjoin X
  def classOf = intToResidueClass(4)

  val zero = classOf(0)
  val one = classOf(1)
  val two = classOf(2)
  val three = classOf(3)


  // Note that this really belongs in PolynomialTest, but it is simply easier to write the test here.
  test("Polynomials defined over different formal parameters are never equal") {

    import polynomial.Predef.Y
    val polyRing2 = intsMod4 r_adjoin Y

    polyRing.zero == polyRing2.zero should be (false)
  }

  test("Addition works") {

    Polynomial(three) + Polynomial(two) should be (Polynomial(one))
    Polynomial(one, three) + Polynomial(two) should be (Polynomial(one, one))
    Polynomial(two, three) + Polynomial(two, two) should be (Polynomial(one))
    Polynomial(three, zero) + polyRing.zero should be (Polynomial(three, zero))
  }

  test("Subtraction works") {

    Polynomial(three) - Polynomial(two) should be (Polynomial(one))
    Polynomial(one, two) - Polynomial(three) should be (Polynomial(one, three))
    Polynomial(two, three) - Polynomial(two, two) should be (Polynomial(one))
    Polynomial(three, zero) - polyRing.zero should be (Polynomial(three, zero))
  }

  test("Multiplication works") {

    Polynomial(three) * Polynomial(two) should be (Polynomial(two))
    Polynomial(one, two) * Polynomial(three) should be (Polynomial(three, two))
    Polynomial(one, three, two) * Polynomial(three, one) should be (Polynomial(three, two, one, two))
    Polynomial(one, two, three) * polyRing.zero should be (polyRing.zero)
  }

  test("Exponentiation works") {

    Polynomial(one, three) ^ 0 should be (polyRing.one)
    Polynomial(one, three) ^ 1 should be (Polynomial(one, three))
    Polynomial(one, three) ^ 2 should be (Polynomial(one, two, one))
    Polynomial(one, three) ^ 3 should be (Polynomial(one, one, three, three))
  }

  test("Negation works") {

    Polynomial(one, three, three).negate should be (Polynomial(three, one, one))
  }

  test("The ring has zero and one") {

    polyRing.zero should be (Polynomial(zero))
    polyRing.one should be (Polynomial(one))
  }
}
