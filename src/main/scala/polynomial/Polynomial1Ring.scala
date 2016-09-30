package polynomial

import algebra.ring.CommutativeRing
import PolynomialHelper.{biggestKeyWithNonzeroValue, mapCoefficientsAndExtend, pairwiseBinaryListOp, trimLeadingZeros}

//TODO: Make this a Euclidean Ring so that we can effectively make rational functions
trait Polynomial1Ring[A] extends CommutativeRing[Polynomial1[A]] {

  def param: FormalParameter

  implicit def ring: CommutativeRing[A]

  //TODO: For the zero case, return negative infinity!
  def degree(poly: Polynomial1[A]): Int = biggestKeyWithNonzeroValue(poly.coefficients, ring).getOrElse(-1)

  def leadingCoefficient(polynomial: Polynomial1[A]): A = {

    val d = degree(polynomial)
    if(d < 0) ring.zero else polynomial.coefficients(d)
  }

  def polynomial(xa: A*): Polynomial1[A] = polynomialFromList(xa.reverse.toList)

  override def plus(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = polynomialFromList(listPlus(x.coefficients, y.coefficients))

  override def times(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = {

    val xWithIndex: List[(A, Int)] = x.coefficients zipWithIndex
    val termsOfXMultipliedByY: List[Polynomial1[A]] = xWithIndex.map({case (a, i) => {
      polynomialFromList(mapCoefficientsAndExtend(a, i, y.coefficients, ring))
    }})

    termsOfXMultipliedByY.fold(zero)(plus(_, _))
  }

  override def one: Polynomial1[A] = polynomial(ring.one)

  override def zero: Polynomial1[A] = polynomial(ring.zero)

  override def negate(x: Polynomial1[A]): Polynomial1[A] = polynomialFromList(listNegate(x.coefficients))

  private def polynomialFromList(coefficients: List[A]): Polynomial1[A] = Polynomial1[A](param, trimLeadingZeros(coefficients, ring))

  private def listPlus(x: List[A], y: List[A]): List[A] = pairwiseBinaryListOp(ring.plus, ring.zero)(x, y)

  private def listNegate(x: List[A]): List[A] = x map ring.negate

}

object Polynomial1Ring {
  def apply[A](p: FormalParameter, r: CommutativeRing[A]): Polynomial1Ring[A] = new Polynomial1Ring[A] {

    override def param: FormalParameter = p

    override def ring: CommutativeRing[A] = r

  }
}

// coefficients(i) is the coefficient for the i_th degree term
case class Polynomial1[A] (val param: FormalParameter, val coefficients: List[A]) {

  def +(other: Polynomial1[A])(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.plus(this, other)

  def -(other: Polynomial1[A])(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.minus(this, other)

  def *(other: Polynomial1[A])(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.times(this, other)

  def ^(exp: Int)(implicit ring: Polynomial1Ring[A]): Polynomial1[A] = ring.pow(this, exp)

  def degree(implicit ring: Polynomial1Ring[A]): Int = ring.degree(this)

  def leadingCoefficient(implicit ring: Polynomial1Ring[A]): A = ring.leadingCoefficient(this)
}