package finitefield

import core.{EuclideanRingModdingContext, ResidueClass}
import polynomial.{ExtendedInteger, FiniteInteger, NegativeInfinity, Polynomial}

object PolynomialOverIntsModPModdingContext {

  def apply(polyRing: PolynomialRingOverIntegersModP): EuclideanRingModdingContext[Polynomial[ResidueClass[Int]]] = new EuclideanRingModdingContext[Polynomial[ResidueClass[Int]]] {

    //TODO: the member "coefficients" makes it look like we're mapping over the coefficients only, not all members of the field.
//    override def elementsUpTo(a: Polynomial[ResidueClass[Int]]): IndexedSeq[Polynomial[ResidueClass[Int]]] = {
//
//      def createLists[A](s: Set[A], l: Int): List[List[A]] = {
//
//        require(l > 0)
//        require(s.nonEmpty)
//
//        def loop(acc: List[List[A]], d: Int): List[List[A]] = d match {
//          case d if d <= 0 => acc
//          case d if d > 0 => {
//            val appended = for {
//              a <- s
//              lst <- acc
//            } yield a :: lst
//
//            loop(appended.toList, d - 1)
//          }
//        }
//
//        loop(s.toList.map(a => List(a)), l - 1)
//      }
//
//      //TODO: This is not the way to pass implicits
//      implicit val pr = polyRing
//
//      val degree = a.degree
//      degree match {
//        case NegativeInfinity => IndexedSeq(polyRing.zero)
//        case FiniteInteger(d) if d >= 0 => createLists(polyRing.coefficients.elements, d).map(coef => Polynomial(polyRing.parameter, coef)(polyRing.coefficients)).toIndexedSeq
//      }
//
//    }

    override def elementsUpTo(a: Polynomial[ResidueClass[Int]]): IndexedSeq[Polynomial[ResidueClass[Int]]] = {

      val nonZeroElements = (polyRing.coefficients.elements - polyRing.coefficients.zero).toIndexedSeq

      a.degree match {

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


//        ListOps.product(polyRing.coefficients.elements.toList, n)
//          .filterNot(lst => lst.head == polyRing.coefficients.zero )
//          .map(coef => Polynomial(polyRing.parameter, coef)(polyRing.coefficients))
//          .toIndexedSeq

      }
    }









  }

}
