package polynomial

import algebra.ring.CommutativeRing
import PolynomialHelper.{biggestKeyWithNonzeroValue, mapCoefficientsAndExtend, pairwiseBinaryListOp, trimLeadingZeros}
import language.postfixOps

trait Polynomial1Ring[A] extends CommutativeRing[Polynomial1[A]] {

  def param: FormalParameter

  implicit def coefficientRing: CommutativeRing[A]

  def polynomial(xa: A*): Polynomial1[A] = polynomialFromList(xa.reverse.toList)

  override def one: Polynomial1[A] = polynomial(coefficientRing.one)

  override def zero: Polynomial1[A] = polynomial(coefficientRing.zero)

  override def plus(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = polynomialFromList(listPlus(x.coefficients, y.coefficients))

  override def negate(x: Polynomial1[A]): Polynomial1[A] = polynomialFromList(listNegate(x.coefficients))

  override def times(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = {

    val xWithIndex: List[(A, Int)] = x.coefficients zipWithIndex
    val termsOfXMultipliedByY: List[Polynomial1[A]] = xWithIndex.map({case (a, i) =>
      polynomialFromList(mapCoefficientsAndExtend(a, i, y.coefficients, coefficientRing))
    })

    termsOfXMultipliedByY.fold(zero)(plus)
  }

  //TODO: For the zero case, return negative infinity!
  def degree(poly: Polynomial1[A]): Int = biggestKeyWithNonzeroValue(poly.coefficients, coefficientRing).getOrElse(-1)

  def leadingCoefficient(polynomial: Polynomial1[A]): A = {

    val d = degree(polynomial)
    if(d < 0) coefficientRing.zero else polynomial.coefficients(d)
  }

  private[polynomial] def polynomialFromList(coefficients: List[A]): Polynomial1[A] = Polynomial1[A](param, trimLeadingZeros(coefficients, coefficientRing))

  private def listPlus(x: List[A], y: List[A]): List[A] = pairwiseBinaryListOp(coefficientRing.plus, coefficientRing.zero)(x, y)

  private def listNegate(x: List[A]): List[A] = x map coefficientRing.negate

}

object Polynomial1Ring {
  def apply[A](p: FormalParameter, r: CommutativeRing[A]): Polynomial1Ring[A] = new Polynomial1Ring[A] {

    override def param: FormalParameter = p

    override def coefficientRing: CommutativeRing[A] = r

  }
}

// coefficients(i) is the coefficient for the i_th degree term
case class Polynomial1[A](param: FormalParameter, coefficients: List[A]) {

  def +(other: Polynomial1[A])(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.plus(this, other)

  def -(other: Polynomial1[A])(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.minus(this, other)

  def *(other: Polynomial1[A])(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.times(this, other)

  def ^(exp: Int)(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.pow(this, exp)

  def degree(implicit ring: Polynomial1Ring[A]): Int = ring.degree(this)

  def leadingCoefficient(implicit ring: Polynomial1Ring[A]): A = ring.leadingCoefficient(this)

  def negate(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.negate(this)
}