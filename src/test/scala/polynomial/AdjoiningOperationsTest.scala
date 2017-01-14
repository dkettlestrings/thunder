package polynomial

import core.IntegerModding._
import core.Integers
import org.scalatest.{FunSuite, Matchers}
import AdjoiningOperations._
import Predef.X

class AdjoiningOperationsTest extends FunSuite with Matchers {

  test("You can create a polynomial ring over the integers") {

    implicit val polyRing = integers r_adjoin X

    val a = Polynomial(2, 3, 1) + Polynomial(2, -1)
    val b = Polynomial(2, 3, 1)
    val c = Polynomial(2, -1)

    Polynomial(0) should be (polyRing.zero)
    Polynomial(2, 3, 1) + Polynomial(2, -1) should be (Polynomial(2, 5, 0))
  }

  test("You can create a polynomial over the integers modulo n") {

    implicit val intsMod4 = IntegersMod(4)
    implicit val polyRing = intsMod4 r_adjoin X
    def classOf = intToResidueClass(4)

    val zero = classOf(0)
    val one = classOf(1)
    val three = classOf(3)
    val five = classOf(5)


    Polynomial(zero) should be (polyRing.zero)
    Polynomial(one, three, one) + Polynomial(three, five, zero) should be (Polynomial(one))

    //It ss just a PolynomialRing, not a PolynomialRingOverField, so it does not have quot for example
    assertDoesNotCompile("polyRing.quot(polyRing.one, polyRing.one)")
  }

  test("You can create a polynomial over the integers modulo p") {

    implicit val intsMod3 = PrimeField(3)
    implicit val polyRing = intsMod3 f_adjoin X
    implicit def classOf = intToResidueClass(3)

    val zero = classOf(0)
    val one = classOf(1)
    val two = classOf(2)
    val three = classOf(3)

    Polynomial(zero) should be (polyRing.zero)
    Polynomial(one, three, one) + Polynomial(two, three, zero) should be (Polynomial(one))

    //Now we have a field for the coefficients, so we should have quot
    polyRing.quot(polyRing.one, polyRing.one) should be (polyRing.one)
  }

}
