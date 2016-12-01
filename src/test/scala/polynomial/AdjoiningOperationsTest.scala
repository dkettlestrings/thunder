package polynomial

import core.Integers
import org.scalatest.{FunSuite, Matchers}
import polynomial.AdjoiningOperations._
import polynomial.Predef.X
import core.IntegerModding._

class AdjoiningOperationsTest extends FunSuite with Matchers {

  test("You can create a polynomial ring over the integers") {

    implicit val polyRing = Integers() adjoin X

    polyRing.polynomial(0) should be (polyRing.zero)
    polyRing.polynomial(2, 3, 1) + polyRing.polynomial(2, -1) should be (polyRing.polynomial(2, 5, 0))
  }

  test("You can create a polynomial over the integers modulo n") {

    implicit val intsMod4 = IntegersMod(4)
    implicit val polyRing = intsMod4 adjoin X
    implicit def converter = intToResidueClass(4)

    polyRing.polynomial(0) should be (polyRing.zero)
    polyRing.polynomial(1, 3, 1) + polyRing.polynomial(3, 5, 0) should be (polyRing.polynomial(1))

    //It ss just a PolynomialRing, not a PolynomialRingOverField, so it does not have quot for example
    assertDoesNotCompile("polyRing.quot(polyRing.one, polyRing.one)")
  }

  test("You can create a polynomial over the integers modulo p") {

    implicit val intsMod3 = PrimeField(3)
    implicit val polyRing = intsMod3 adjoin X
    implicit def converter = intToResidueClass(3)

    polyRing.polynomial(0) should be (polyRing.zero)
    polyRing.polynomial(1, 3, 1) + polyRing.polynomial(2, 3, 0) should be (polyRing.polynomial(1))

    //Now we have a field for the coefficients, so we should have quot
    polyRing.quot(polyRing.one, polyRing.one) should be (polyRing.one)
  }

}
