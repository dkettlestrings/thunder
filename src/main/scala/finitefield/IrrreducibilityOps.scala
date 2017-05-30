package finitefield

import core.ResidueClass
import polynomial.{FiniteInteger, Polynomial}

object IrrreducibilityOps {

  def monicIrreduciblePolynomialsOfDegree(n: Int, polyRing: PolynomialRingOverIntegersModP): Set[Polynomial[ResidueClass[Int]]] = {

    require(n >= 0)

    implicit val coefficientField = polyRing.coefficients

    n match {

      case 0 => Set(Polynomial(polyRing.parameter, coefficientField.one))
      case d if d >=1 =>

        val context = PolynomialOverIntsModPModdingContext(polyRing)

        val coefficients = polyRing.coefficients.one :: List.fill(n)(polyRing.coefficients.one)

        val allNonConstantPolysUpToDegreeN = context.elementsUpTo(Polynomial(polyRing.parameter, coefficients))
                                                    .filter(_.degree != FiniteInteger(0))

        val monicPolysOfDegreeN = allNonConstantPolysUpToDegreeN
                                    .filter(p =>
                                      p.degree == FiniteInteger(n) &&
                                      p.leadingCoefficient == polyRing.coefficients.one)

        val nonConstantPolysOfDegreeLessThanN = allNonConstantPolysUpToDegreeN.filter(_.degree < FiniteInteger(n))

        val irreducibles = monicPolysOfDegreeN.filter(mp => nonConstantPolysOfDegreeLessThanN.forall(p => polyRing.mod(mp, p) != polyRing.zero))

        irreducibles.toSet


    }
  }



}
