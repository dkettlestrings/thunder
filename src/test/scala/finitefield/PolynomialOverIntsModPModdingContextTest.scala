package finitefield

import org.scalatest.{FunSuite, Matchers}
import polynomial.Polynomial
import polynomial.Predef.X

class PolynomialOverIntsModPModdingContextTest extends FunSuite with Matchers {

  val polyRing = PolynomialRingOverIntegersModP(3, X)
  implicit val coefficientField = polyRing.coefficients
  val zero = coefficientField.zero
  val one = coefficientField.one
  val two = coefficientField.classOf(2)

  val ctxt = PolynomialOverIntsModPModdingContext(polyRing)

  val polynomialsOfDegreeZero = Set(Polynomial(one), Polynomial(two))

  val polynomialsOfDegreeOne = Set(Polynomial(one, one), Polynomial(one, two), Polynomial(one, zero),
                                  Polynomial(two, one), Polynomial(two, two), Polynomial(two, zero))

  test("Elements up to the zero polynomial should be undefined") {

    intercept[ArithmeticException](ctxt.elementsUpTo(polyRing.zero))
  }

  test("Elements up to a constant polynomial should be all polynomials of degree 0") {

    polynomialsOfDegreeZero.foreach(poly => ctxt.elementsUpTo(poly).toSet should be (polynomialsOfDegreeZero))

  }

  // TODO: Must fix immediately!  Set equality is needed!
  test("Elements up to a polynomial of degree one should be all polynomials of degree zero followed by all polynomials of degree one") {

    polynomialsOfDegreeOne.foreach(poly => {

      val polysUpTo = ctxt.elementsUpTo(poly)

      val (degreeZero, degreeOne) = polysUpTo.splitAt(2)


      degreeZero.toSet should be (polynomialsOfDegreeZero)
      degreeOne.toSet should be (polynomialsOfDegreeOne)


    })


  }


}
