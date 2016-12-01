package polynomial

import algebra.ring.CommutativeRing
import PolynomialHelper.{biggestKeyWithNonzeroValue, mapCoefficientsAndExtend, pairwiseBinaryListOp, trimLeadingZeros}
import core.{ExtendedInteger, FiniteInteger, NegativeInfinity, PositiveInfinity}

import language.postfixOps

trait PolynomialRing[A] extends CommutativeRing[Polynomial[A]] {

  def param: FormalParameter

  implicit def coefficientRing: CommutativeRing[A]

  def polynomial(xa: A*): Polynomial[A] = polynomialFromList(xa.reverse.toList)

  override def one: Polynomial[A] = polynomial(coefficientRing.one)

  override def zero: Polynomial[A] = polynomial(coefficientRing.zero)

  override def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = polynomialFromList(listPlus(x.coefficients, y.coefficients))

  override def negate(x: Polynomial[A]): Polynomial[A] = polynomialFromList(listNegate(x.coefficients))

  override def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {

    val xWithIndex: List[(A, Int)] = x.coefficients zipWithIndex
    val termsOfXMultipliedByY: List[Polynomial[A]] = xWithIndex.map({case (a, i) =>
      polynomialFromList(mapCoefficientsAndExtend(a, i, y.coefficients, coefficientRing))
    })

    termsOfXMultipliedByY.fold(zero)(plus)
  }

  def degree(poly: Polynomial[A]): ExtendedInteger = {
    biggestKeyWithNonzeroValue(poly.coefficients, coefficientRing) match {
      case Some(a) => FiniteInteger(a)
      case None => NegativeInfinity
    }
  }

  def leadingCoefficient(polynomial: Polynomial[A]): A = {

    val d = degree(polynomial)
    d match {
      case NegativeInfinity => coefficientRing.zero
      case FiniteInteger(a) => polynomial.coefficients(a)
    }
  }

  private[polynomial] def polynomialFromList(coefficients: List[A]): Polynomial[A] = Polynomial[A](param, trimLeadingZeros(coefficients, coefficientRing))

  private def listPlus(x: List[A], y: List[A]): List[A] = pairwiseBinaryListOp(coefficientRing.plus, coefficientRing.zero)(x, y)

  private def listNegate(x: List[A]): List[A] = x map coefficientRing.negate

}

object PolynomialRing {
  def apply[A](p: FormalParameter, r: CommutativeRing[A]): PolynomialRing[A] = new PolynomialRing[A] {

    override def param: FormalParameter = p

    override def coefficientRing: CommutativeRing[A] = r

  }
}

// coefficients(i) is the coefficient for the i_th degree term
case class Polynomial[A](param: FormalParameter, coefficients: List[A]) {

  def +(other: Polynomial[A])(implicit ring: PolynomialRing[A]): Polynomial[A] = ring.plus(this, other)

  def -(other: Polynomial[A])(implicit ring: PolynomialRing[A]): Polynomial[A] = ring.minus(this, other)

  def *(other: Polynomial[A])(implicit ring: PolynomialRing[A]): Polynomial[A] = ring.times(this, other)

  def ^(exp: Int)(implicit ring: PolynomialRing[A]): Polynomial[A] = ring.pow(this, exp)

  def degree(implicit ring: PolynomialRing[A]): ExtendedInteger = ring.degree(this)

  def leadingCoefficient(implicit ring: PolynomialRing[A]): A = ring.leadingCoefficient(this)

  def negate(implicit ring: PolynomialRing[A]): Polynomial[A] = ring.negate(this)
}