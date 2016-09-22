package polynomial

import finitefields.{Converter, IntegersMod, PrimeField, ResidueClass}
import org.scalatest.{FunSuite, Matchers}
import polynomial.Predef.X

class Polynomial1Test extends FunSuite with Matchers {

  test("You can create a polynomial over a ring") {

    val intsMod4 = IntegersMod(4)
    val poly = Polynomial1(X, Map(0 -> intsMod4.residueClass(1)))
    poly should be
  }

  test("Actually, you can create a polynomial over anything you want (but it's usually dumb)") {

    val poly = Polynomial1(X, Map(0 -> "foo"))
    poly should be
  }

  test("You can only create a polynomial ring over a commutative ring") {

    val intsMod4 = IntegersMod(4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4)
    polynomial1Ring should be
  }

  test("While direct polynomial construction is allowed, it is reccomended you do so from within the context of a polynomial ring") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4)
    val poly = polynomial1Ring.polynomial(1, 2, 3) // 1*X^2 + 2*X + 3
    poly should be
  }

  test("You can also specify the coefficients as a map from the degree of the term to the value of the coefficient") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4)
    val coefficients: Map[Int, ResidueClass] = Map(2 -> 1, 1 -> 2, 0 -> 3) // 1*X^2 + 2*X + 3
    val poly = polynomial1Ring.polynomial(coefficients)
    poly should be
  }

  test("Of course, you can also create polynomial rings over a field too") {

    val intsMod5 = PrimeField(5)
    val polys = Polynomial1Ring(X, intsMod5)
    polys should be
  }

  test("Polynomials have a degree within the context of a PolynomialRing") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val poly1 = polyRing.polynomial(4, 2, 0, 1)

    polyRing.degree(poly1) should be (3)
  }

  test("Leading zeros don't affect the degree") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val poly1 = polyRing.polynomial(0, 4, 2, 0, 1)

    polyRing.degree(poly1) should be (3)
  }

  test("Constant polynomials have degree 0") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val poly1 = polyRing.polynomial(3)

    polyRing.degree(poly1) should be (0)
  }

  //TODO: No they don't, it's negative infinity!
  test("By convention, the zero polynomial has degree 0") {

    val intsMod5 = PrimeField(5)
    val polyRing = Polynomial1Ring(X, intsMod5)

    polyRing.degree(polyRing.zero) should be (0)
  }
}
