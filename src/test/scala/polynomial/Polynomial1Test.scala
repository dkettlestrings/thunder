package polynomial

import finitefields.{Converter, IntegersMod, PrimeField}
import org.scalatest.{FunSuite, Matchers}
import polynomial.Predef.X

class Polynomial1Test extends FunSuite with Matchers {

  test("You can create a polynomial over a ring") {

    val intsMod4 = IntegersMod(4)
    val poly = Polynomial1(X, Map(0 -> intsMod4.residueClass(1))) should be
  }

  test("Actually, you can create a polynomial over anything you want (but it's usually dumb)") {

    val poly = Polynomial1(X, Map(0 -> "foo")) should be
  }

  test("You can only create a polynomial ring over a commutative ring") {

    val intsMod4 = IntegersMod(4)
    val polynomial1Ring = Polynomial1Ring(X, intsMod4) should be
  }

  test("Of course, you can also create polynomial rings over a field too") {

    val intsMod5 = PrimeField(5)
    val polys = Polynomial1Ring(X, intsMod5) should be
  }

  test("Polynomials have a degree within the context of a PolynomialRing") {

    val intsMod5 = PrimeField(5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val poly1 = Polynomial1(X, Map(0 -> intsMod5.residueClass(1), 1 -> intsMod5.residueClass(2), 3 -> intsMod5.residueClass(4)))

    polyRing.degree(poly1) should be (3)
  }

  test("Constant polynomials have degree 0") {

    val intsMod5 = PrimeField(5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val poly1 = Polynomial1(X, Map(0 -> intsMod5.residueClass(5)))

    polyRing.degree(poly1) should be (0)
  }

  //TODO: No they don't, it's negative infinity!
  test("By convention, the zero polynomial has degree 0") {

    val intsMod5 = PrimeField(5)
    val polyRing = Polynomial1Ring(X, intsMod5)

    polyRing.degree(polyRing.zero) should be (0)
  }

  test("The polynomial ring has a utility method for creating polynomials in the ring") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    val polyRing = Polynomial1Ring(X, intsMod5)
    val poly = polyRing.polynomial(1, 5)

    polyRing.degree(poly) should be (1)
  }



}
