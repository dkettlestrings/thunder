package finitefield

import polynomial.Predef.X
import IrrreducibilityOps._
import org.scalatest.{FunSuite, Matchers}
import polynomial.Polynomial

class IrreducibilityOpsTest extends FunSuite with Matchers {

  val polyRing = PolynomialRingOverIntegersModP(5, X)
  implicit val coefficientField = polyRing.coefficients

  val zero = polyRing.coefficients.zero
  val one = polyRing.coefficients.one

  test("The irreducible monic polynomial of degree 0 is the constant polynomial") {

    val p = monicIrreduciblePolynomialOfDegree(0, polyRing)
    p should be (polyRing.one)
  }

  test("The monic irreducible polynomial of degree 1 should be X") {

    val p = monicIrreduciblePolynomialOfDegree(1, polyRing)
    p should be (Polynomial(one))
  }

}
