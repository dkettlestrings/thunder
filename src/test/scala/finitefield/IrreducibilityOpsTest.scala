package finitefield

import finitefield.IrrreducibilityOps._
import org.scalatest.{FunSuite, Matchers}
import polynomial.Polynomial
import polynomial.Predef.X

class IrreducibilityOpsTest extends FunSuite with Matchers {

  val polyRing = PolynomialRingOverIntegersModP(3, X)
  implicit val coefficientField = polyRing.coefficients

  val zero = polyRing.coefficients.zero
  val one = polyRing.coefficients.one
  val two = polyRing.coefficients classOf 2

  test("Degree 0") {

    val result = monicIrreduciblePolynomialsOfDegree(0, polyRing)

    val expected = Set(Polynomial(one))

    result should be (expected)
  }

  test("Degree 1") {

    val result = monicIrreduciblePolynomialsOfDegree(1, polyRing)

    val expected = Set(
      Polynomial(one, zero),
      Polynomial(one, one),
      Polynomial(one, two)
    )

    result should be (expected)
  }

  test("Degree 2") {

    val result = monicIrreduciblePolynomialsOfDegree(2, polyRing)

    val expected = Set(
      Polynomial(one, zero, one),
      Polynomial(one, one, two),
      Polynomial(one, two, two)
    )

    result should be (expected)
  }

  test("Degree 3") {

    val result = monicIrreduciblePolynomialsOfDegree(3, polyRing)

    val expected = Set(
      Polynomial(one, zero, two, one),
      Polynomial(one, zero, two, two),
      Polynomial(one, one, zero, two),
      Polynomial(one, one, one, two),
      Polynomial(one, one, two, one),
      Polynomial(one, two, zero, one),
      Polynomial(one, two, one, one),
      Polynomial(one, two, two, two)
    )

    result should be (expected)
  }

  // We can calculate the number a priori using Dummit and Foote, Chapter 14, Galois theory, Pages 567−568
  test("Degree 4") {

    val result = monicIrreduciblePolynomialsOfDegree(4, polyRing)

    result.size should be (18)
  }

}
