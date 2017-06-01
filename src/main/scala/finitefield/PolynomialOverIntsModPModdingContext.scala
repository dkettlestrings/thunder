package finitefield

import core.{EuclideanRingModdingContext, ResidueClass}
import polynomial.{FiniteInteger, NegativeInfinity, Polynomial}

object PolynomialOverIntsModPModdingContext {

  def apply(polyRing: PolynomialRingOverIntegersModP): EuclideanRingModdingContext[Polynomial[ResidueClass[Int]]] = new EuclideanRingModdingContext[Polynomial[ResidueClass[Int]]] {

    override def elementsUpTo(a: Polynomial[ResidueClass[Int]]): IndexedSeq[Polynomial[ResidueClass[Int]]] = {

      val nonZeroElements = (polyRing.coefficients.elements - polyRing.coefficients.zero).toIndexedSeq

      // The @unchecked is to suppress a compiler warning for not matching against PositiveInfinity, which is impossible
      (a.degree: @unchecked) match {

        case NegativeInfinity => throw new ArithmeticException("elementsUpTo(zero) is undefined")
        case FiniteInteger(d) => (0 to d).flatMap( n => {

          val degreeN = polynomialsOfDegree(n)
          degreeN
        })
      }
    }

    def polynomialsOfDegree(n: Int): IndexedSeq[Polynomial[ResidueClass[Int]]] = {

      val nonZeroElements = (polyRing.coefficients.elements - polyRing.coefficients.zero).toIndexedSeq

      if (n == 0) nonZeroElements.map(residueClass => Polynomial(polyRing.parameter, List(residueClass))(polyRing.coefficients))
      else {

        val a = ListOps.product(polyRing.coefficients.elements.toList, n + 1)
        val b = a.filterNot(lst => lst.head == polyRing.coefficients.zero )
        val c = b.map(coef => Polynomial(polyRing.parameter, coef)(polyRing.coefficients))
        val d = c.toIndexedSeq
        d

      }
    }


  }

}
