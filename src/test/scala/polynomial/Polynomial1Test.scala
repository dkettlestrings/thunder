package polynomial

import finitefields.{Converter, IntegersMod, PrimeField, ResidueClass}
import org.scalatest.{FunSuite, Matchers}
import polynomial.Predef.X

class Polynomial1Test extends FunSuite with Matchers {

  test("You can create a polynomial over a ring") {

    val intsMod4 = IntegersMod(4)
    val poly = Polynomial1(X, List(intsMod4.residueClass(0), intsMod4.residueClass(1)))
    poly should be
  }

  test("Actually, you can create a polynomial over anything you want (but it's usually dumb)") {

    val poly = Polynomial1(X, List("foo"))
    poly should be
  }

  test("You can only create a polynomial ring over a commutative ring") {

    val intsMod4 = IntegersMod(4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4)
    polynomial1Ring should be
  }

  test("While direct polynomial construction is allowed, it is recommended you do so from within the context of a polynomial ring") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4)
    val poly = polynomial1Ring.polynomial(1, 2, 3) // 1*X^2 + 2*X + 3
    poly should be
  }

  test("When constructing polynomials from within a polynomial ring, leading zero coefficients are ignored") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4)
    val poly1 = polynomial1Ring.polynomial(1, 2, 0, 3)
    val poly2 = polynomial1Ring.polynomial(0, 0, 1, 2, 0, 3)

    poly1 == poly2 should be (true)
  }

  test("Similarly, you can also ask for the leading coefficient of a polynomial.  It will be the zero element of the ring only when the polynomial is the zero polynomial") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4)
    val poly1 = polynomial1Ring.polynomial(1, 2, 3)
    val poly2 = polynomial1Ring.polynomial(0, 1, 2, 3)
    val zero = polynomial1Ring.zero

    polynomial1Ring.leadingCoefficient(poly1) should be (intsMod4.residueClass(1))
    polynomial1Ring.leadingCoefficient(poly2) should be (intsMod4.residueClass(1))
    polynomial1Ring.leadingCoefficient(zero) should be (intsMod4.residueClass(0))
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
  test("By convention, the zero polynomial has degree -1") {

    val intsMod5 = PrimeField(5)
    val polyRing = Polynomial1Ring(X, intsMod5)

    polyRing.degree(polyRing.zero) should be (-1)
  }

  test("Polynomials have addition, subtraction, and negation") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val p1 = polyRing.polynomial(1, 3, 2)
    val p2 = polyRing.polynomial(4, 1)

    polyRing.plus(p1, p2) should be (polyRing.polynomial(1, 2, 3))

    polyRing.minus(p1, p2) should be (polyRing.polynomial(1, 4, 1))
    polyRing.minus(p2, p1) should be (polyRing.polynomial(4, 1, 4))

    polyRing.negate(p1) should be (polyRing.polynomial(4, 2, 3))
  }

  test("Polynomials also have multiplication") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val p1 = polyRing.polynomial(1, 3, 2)
    val p2 = polyRing.polynomial(4, 1)

    polyRing.times(p1, p2) should be (polyRing.polynomial(4, 3, 1, 2))
    polyRing.times(p2, p2) should be (polyRing.polynomial(1, 3, 1))
  }

  test("Of course the ring of polynomials has all of the ring structure you expect") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val p1 = polyRing.polynomial(1, 3)
    val p2 = polyRing.polynomial(4, 1)
    val p3 = polyRing.polynomial(3, 2)


    polyRing.one should be (polyRing.polynomial(1))
    polyRing.zero should be (polyRing.polynomial(0))

    polyRing.plus(p1, polyRing.zero) should be (p1)
    polyRing.plus(polyRing.zero, p1) should be (p1)

    polyRing.plus(polyRing.negate(p1), p1) should be (polyRing.zero)

    polyRing.plus(p1, p2) should be (polyRing.plus(p2, p1))

    polyRing.times(p1, polyRing.one) should be (p1)
    polyRing.times(polyRing.one, p1) should be (p1)

    polyRing.times(p1, p2) should be (polyRing.times(p2, p1))

    polyRing.times(p1, polyRing.plus(p2, p3)) should be (polyRing.plus(polyRing.times(p1, p2), polyRing.times(p1, p3)))
  }
}
