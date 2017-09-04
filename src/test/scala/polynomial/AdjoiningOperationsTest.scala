package polynomial

import integer.{IntegersModN, IntegersModP, integers}
import org.scalatest.{FunSuite, Matchers}
import polynomial.AdjoiningOperations._
import polynomial.Predef.X
import core.CommutativeRingOps._

class AdjoiningOperationsTest extends FunSuite with Matchers {

  test("You can create a polynomial ring over the integers") {

    implicit val polyRing = integers r_adjoin X

    Polynomial(0) should be (polyRing.zero)
    Polynomial(2, 3, 1) + Polynomial(2, -1) should be (Polynomial(2, 5, 0))
  }

  test("You can create a polynomial over the integers modulo n") {

    implicit val intsMod4 = IntegersModN(4)
    implicit val polyRing = intsMod4 r_adjoin X

    val zero = intsMod4.classOf(0)
    val one = intsMod4.classOf(1)
    val three = intsMod4.classOf(3)
    val five = intsMod4.classOf(5)


    Polynomial(zero) should be (polyRing.zero)
    Polynomial(one, three, one) + Polynomial(three, five, zero) should be (Polynomial(one))

    //It is just a PolynomialRing, not a PolynomialRingOverField, so it does not have quot for example
    assertDoesNotCompile("polyRing.quot(polyRing.one, polyRing.one)")
  }

  test("You can create a polynomial over the integers modulo p") {

    implicit val intsMod3 = IntegersModP(3)
    implicit val polyRing = intsMod3 f_adjoin X

    val zero = intsMod3.classOf(0)
    val one = intsMod3.classOf(1)
    val two = intsMod3.classOf(2)
    val three = intsMod3.classOf(3)

    Polynomial(zero) should be (polyRing.zero)
    Polynomial(one, three, one) + Polynomial(two, three, zero) should be (Polynomial(one))

    //Now we have a field for the coefficients, so we should have quot
    polyRing.quot(polyRing.one, polyRing.one) should be (polyRing.one)
  }

}
