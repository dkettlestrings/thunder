package polynomial

import core.{FiniteInteger, NegativeInfinity}
import integer.IntegersModN
import polynomial.Predef.{X, Y}
import org.scalatest.{FunSuite, Matchers}

class Polynomial2Test extends FunSuite with Matchers {

  implicit val intsMod4 = IntegersModN(4)

  val zero = intsMod4.classOf(0)
  val one = intsMod4.classOf(1)
  val two = intsMod4.classOf(2)
  val three = intsMod4.classOf(3)

  test("You can create a polynomial in two variables using a map") {

    val poly = Polynomial2(X, Y, Map((1, 3) -> one, (2, 1) -> two)) // one * X * Y^3 + two * X^2 * Y
  }

  test("Polynomials in two variables are equal if their coefficients match (disregarding zeros") {

    val poly1 = Polynomial2(X, Y, Map((1, 3) -> one, (2, 1) -> two))
    val poly2 = Polynomial2(X, Y, Map( (2, 1) -> two, (4, 4) -> zero, (1, 3) -> one))

    poly1 == poly2 should be (true)
  }

  test("Polynomials in two variables respect the formal parameters in equality") {

    val poly1 = Polynomial2(X, Y, Map((1, 3) -> one, (2, 1) -> two))
    val poly2 = Polynomial2(Y, X, Map((1, 3) -> one, (2, 1) -> two))

    poly1 == poly2 should be (false)
  }

  test("You can't pass in negative powers to a polynomial in two variables") {

    intercept[IllegalArgumentException](Polynomial2(X, Y, Map((3, 2) -> two, (-1, 6) -> three)))
  }

  test("By convention, the zero polynomial has degree negative infinity") {

    val poly = Polynomial2(X, Y, Map((0, 0) -> zero))
    poly.degree should be (NegativeInfinity)
  }

  test("The degree of nonzero polynomials is the highest sum of degrees of a single term (with nonzero coefficient)") {

    val poly = Polynomial2(X, Y, Map((5, 6) -> two, (1, 9) -> three, (25, 25) -> zero))
    poly.degree should be (FiniteInteger(11))
  }

  test("The hashcode respects equality") {

    val poly1 = Polynomial2(X, Y, Map((1, 3) -> one, (2, 1) -> two))
    val poly2 = Polynomial2(X, Y, Map( (2, 1) -> two, (4, 4) -> zero, (1, 3) -> one))

    poly1.hashCode == poly2.hashCode should be (true)
  }

}
