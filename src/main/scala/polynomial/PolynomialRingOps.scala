package polynomial

import algebra.ring.CommutativeRing
import PolynomialHelper._

import language.postfixOps

private [polynomial] trait PolynomialRingOps[A] {

  def coefficientRing: CommutativeRing[A]

  def param: FormalParameter

  def polynomial(xa: A*): Polynomial[A] = polynomialFromList(xa.reverse.toList)

  def negate(x: Polynomial[A]): Polynomial[A] = polynomialFromList(listNegate(x.coefficients))

  def zero: Polynomial[A] = polynomial(coefficientRing.zero)

  def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = polynomialFromList(listPlus(x.coefficients, y.coefficients))

  def one: Polynomial[A] = polynomial(coefficientRing.one)

  def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {

    val xWithIndex: List[(A, Int)] = x.coefficients zipWithIndex
    val termsOfXMultipliedByY: List[Polynomial[A]] = xWithIndex.map({case (a, i) =>
      polynomialFromList(mapCoefficientsAndExtend(a, i, y.coefficients, coefficientRing))
    })

    termsOfXMultipliedByY.fold(zero)(plus)
  }

  private def polynomialFromList(coefficients: List[A]): Polynomial[A] = Polynomial[A](param, trimLeadingZeros(coefficients, coefficientRing))

  private def listPlus(x: List[A], y: List[A]): List[A] = pairwiseBinaryListOp(coefficientRing.plus, coefficientRing.zero)(x, y)

  private def listNegate(x: List[A]): List[A] = x map coefficientRing.negate

}
