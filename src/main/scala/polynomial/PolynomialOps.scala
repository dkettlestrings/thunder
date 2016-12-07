package polynomial

import core.{ExtendedInteger, FiniteInteger, NegativeInfinity}
import PolynomialHelper._
import algebra.ring.CommutativeRing

import language.implicitConversions

/**
  * A collection of convenient operations for creating and operating on, [[Polynomial]]s.
  */
object PolynomialOps {

  /**
    * Creates a [[Polynomial]] from a variable number of coefficients.
    *
    * Note that when using this method, coefficients are expected in decreasing order by degree.  That is,
    * for 2 * (X ** 2) + X + 7 we would do polynomial(2, 1, 7).
    * @param xa
    * @param param
    * @param coefficientRing
    * @tparam A
    * @return
    */
  def polynomial[A](xa: A*)(implicit param: FormalParameter, coefficientRing: CommutativeRing[A]): Polynomial[A] = polynomialFromList(xa.reverse.toList)

  implicit def toOperablePolynomial[A](poly: Polynomial[A]): OperablePolynomial[A] = new OperablePolynomial[A] {

    override def p: Polynomial[A] = poly
  }



  trait OperablePolynomial[A] {

    def p: Polynomial[A]

    /**
      * The degree of the [[Polynomial]].
      *
      * The degree is defined as the largest exponent attached to a non-zero coefficient.  If all coefficients are zero
      * (i.e. when it is the zero [[Polynomial]]), [[core.NegativeInfinity]] is returned.
      * @param coefficientRing
      * @return
      */
    def degree(implicit coefficientRing: CommutativeRing[A]): ExtendedInteger = {
      biggestKeyWithNonzeroValue(p.coefficients, coefficientRing) match {
        case Some(a) => FiniteInteger(a)
        case None => NegativeInfinity
      }
    }

    // The @unchecked is to suppress a compiler warning for not matching against PositiveInfinity, which is impossible
    /**
      * The coefficient attached to the highest-degree term.
      *
      * This value is guaranteed to be zero (in the ring of coefficients) if and only if the [[Polynomial]] is the zero
      * polynomial.
      * @param coefficientRing
      * @return
      */
    def leadingCoefficient(implicit coefficientRing: CommutativeRing[A]): A = (degree: @unchecked) match {
        case NegativeInfinity => coefficientRing.zero
        case FiniteInteger(a) => p.coefficients(a)
      }

  }

}
