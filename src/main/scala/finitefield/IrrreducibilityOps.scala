package finitefield

import core.ResidueClass
import polynomial.{FiniteInteger, Polynomial}

object IrrreducibilityOps {

  def monicIrreduciblePolynomialOfDegree(n: Int, polyRing: PolynomialRingOverIntegersModP): Polynomial[ResidueClass[Int]] = {

    implicit val coefficientField = polyRing.coefficients

    val ctxt = PolynomialOverIntsModPModdingContext(polyRing)

    val coefficients = polyRing.coefficients.one :: List.fill(n)(polyRing.coefficients.one)
    val allPolysUpToDegreeN = ctxt.elementsUpTo(Polynomial(polyRing.parameter, coefficients))

    val monicPolysOfDegreeN = allPolysUpToDegreeN.filter(p => p.degree == FiniteInteger(n) && p.leadingCoefficient == polyRing.coefficients.one)
    val polysOfDegreeLessThanN = allPolysUpToDegreeN.filter(_.degree < FiniteInteger(n))

    //Find a monic polynomial of degree n that is not divisible by polynomials of degree less than n
    val irreducible = monicPolysOfDegreeN.find(mp => polysOfDegreeLessThanN.forall(p => polyRing.mod(mp, p) != polyRing.zero))
    irreducible match {
      case Some(polynomial) => polynomial
      case None => throw new ArithmeticException(s"No monic irreducible polynomial of degree $n was found.  This should be impossible.")
    }
  }

}
