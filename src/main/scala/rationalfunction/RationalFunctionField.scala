package rationalfunction

import algebra.ring.Field
import polynomial.{FormalParameter, Polynomial1, Polynomial1Ring}

trait RationalFunctionField[A] extends Field[RationalFunction[A]] {

  def polynomialRing: Polynomial1Ring[A]

  def rationalFunction(num: Polynomial1[A], denom: Polynomial1[A]): RationalFunction[A] = RationalFunction[A](polynomialRing.param, num, denom)

  def inv(x: RationalFunction[A]): RationalFunction[A] = {
    if(x == zero) throw new ArithmeticException("Dividing by zero")
    rationalFunction(x.denominator, x.numerator)
  }

  override def one: RationalFunction[A] = rationalFunction(polynomialRing.one, polynomialRing.one)

  override def zero: RationalFunction[A] = rationalFunction(polynomialRing.zero, polynomialRing.one)

  override def plus(x: RationalFunction[A], y: RationalFunction[A]): RationalFunction[A] = {

    val newNum = polynomialRing.plus(polynomialRing.times(x.numerator, y.denominator), polynomialRing.times(y.numerator, x.denominator))
    val newDenom = polynomialRing.times(x.denominator, y.denominator)
    rationalFunction(newNum, newDenom)
  }

  override def negate(x: RationalFunction[A]): RationalFunction[A] = rationalFunction(polynomialRing.negate(x.numerator), x.denominator)

  override def times(x: RationalFunction[A], y: RationalFunction[A]): RationalFunction[A] = {

    val newNum = polynomialRing.times(x.numerator, y.numerator)
    val newDenom = polynomialRing.times(x.denominator, y.denominator)
    rationalFunction(newNum, newDenom)
  }

  override def div(x: RationalFunction[A], y: RationalFunction[A]): RationalFunction[A] = times(x, inv(y))

  //TODO: Get non to use default implementations
  override def quot(x: RationalFunction[A], y: RationalFunction[A]): RationalFunction[A] = {
    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else if(x == zero) zero
    else one
  }

  override def mod(x: RationalFunction[A], y: RationalFunction[A]): RationalFunction[A] = {
    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else zero
  }
}

object RationalFunctionField {

  def apply[A](polynomials: Polynomial1Ring[A]): RationalFunctionField[A] = new RationalFunctionField[A] {

    override def polynomialRing: Polynomial1Ring[A] = polynomials
  }
}

case class RationalFunction[A](val param: FormalParameter, val numerator: Polynomial1[A], denominator: Polynomial1[A])
