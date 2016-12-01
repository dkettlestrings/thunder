package polynomial

import core.{ExtendedInteger, FiniteInteger, NegativeInfinity}
import PolynomialHelper._
import algebra.ring.CommutativeRing

import language.implicitConversions

object PolynomialOps {

  def polynomial[A](xa: A*)(implicit param: FormalParameter, coefficientRing: CommutativeRing[A]): Polynomial[A] = polynomialFromList(xa.reverse.toList)

  implicit def toOperablePolynomial[A](poly: Polynomial[A]): OperablePolynomial[A] = new OperablePolynomial[A] {

    override def p: Polynomial[A] = poly
  }



  trait OperablePolynomial[A] {

    def p: Polynomial[A]

    def degree(implicit coefficientRing: CommutativeRing[A]): ExtendedInteger = {
      biggestKeyWithNonzeroValue(p.coefficients, coefficientRing) match {
        case Some(a) => FiniteInteger(a)
        case None => NegativeInfinity
      }
    }

    // The @unchecked is to suppress a compiler warning for not matching against PositiveInfinity, which is impossible
    def leadingCoefficient(implicit coefficientRing: CommutativeRing[A]): A = (degree: @unchecked) match {
        case NegativeInfinity => coefficientRing.zero
        case FiniteInteger(a) => p.coefficients(a)
      }

  }

}
