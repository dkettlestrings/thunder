package rationalfunction

import algebra.ring.Field
import polynomial.{FormalParameter, Polynomial1, Polynomial1RingOverField}

trait RationalFunctionField[A, B <: Field[A]] extends Field[RationalFunction[A, B]] {

  def param: FormalParameter

  implicit def fieldOfConstants: B

  implicit def polynomialRing: Polynomial1RingOverField[A, B] = Polynomial1RingOverField(param, fieldOfConstants)

  def rationalFunction(num: Polynomial1[A], denom: Polynomial1[A]): RationalFunction[A, B] = {
    if(denom == polynomialRing.zero) throw new ArithmeticException("Dividing by zero")
    RationalFunction[A, B](polynomialRing.param, num, denom)
  }

  override def one: RationalFunction[A, B] = rationalFunction(polynomialRing.one, polynomialRing.one)

  override def zero: RationalFunction[A, B] = rationalFunction(polynomialRing.zero, polynomialRing.one)

  override def plus(x: RationalFunction[A, B], y: RationalFunction[A, B]): RationalFunction[A, B] = {

    val newNum = (x.numerator * y.denominator) + (y.numerator * x.denominator)
    val newDenom = x.denominator * y.denominator
    rationalFunction(newNum, newDenom)
  }

  override def negate(x: RationalFunction[A, B]): RationalFunction[A, B] = rationalFunction(x.numerator.negate, x.denominator)

  override def times(x: RationalFunction[A, B], y: RationalFunction[A, B]): RationalFunction[A, B] = {

    val newNum = x.numerator * y.numerator
    val newDenom = x.denominator * y.denominator
    rationalFunction(newNum, newDenom)
  }

  def inv(x: RationalFunction[A, B]): RationalFunction[A, B] = rationalFunction(x.denominator, x.numerator)

  override def div(x: RationalFunction[A, B], y: RationalFunction[A, B]): RationalFunction[A, B] = times(x, inv(y))

  def areEqual(x: RationalFunction[A, B], y: RationalFunction[A, B]): Boolean = x.denominator * y.numerator == x.numerator * y.denominator

  //TODO: Get non to use default implementations
  override def quot(x: RationalFunction[A, B], y: RationalFunction[A, B]): RationalFunction[A, B] = {
    implicit def field = this
    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else if(x == zero) zero
    else one
  }

  override def mod(x: RationalFunction[A, B], y: RationalFunction[A, B]): RationalFunction[A, B] = {
    implicit def field = this
    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else zero
  }
}

object RationalFunctionField {

  def apply[A, B <: Field[A]](p: FormalParameter, constants: B): RationalFunctionField[A, B] = new RationalFunctionField[A, B] {

    override def param = p

    override implicit def fieldOfConstants: B = constants

  }
}

case class RationalFunction[A, B <: Field[A]](param: FormalParameter, numerator: Polynomial1[A], denominator: Polynomial1[A]) {

  def +(other: RationalFunction[A, B])(implicit field: RationalFunctionField[A, B]): RationalFunction[A, B] = field.plus(this, other)

  def -(other: RationalFunction[A, B])(implicit field: RationalFunctionField[A, B]): RationalFunction[A, B] = field.minus(this, other)

  def negate(implicit field: RationalFunctionField[A, B]): RationalFunction[A, B] = field.negate(this)

  def *(other: RationalFunction[A, B])(implicit field: RationalFunctionField[A, B]): RationalFunction[A, B] = field.times(this, other)

  def /(other: RationalFunction[A, B])(implicit field: RationalFunctionField[A, B]): RationalFunction[A, B] = field.div(this, other)

  def inv(implicit field: RationalFunctionField[A, B]): RationalFunction[A, B] = field.inv(this)

  def ^(exp: Int)(implicit field: RationalFunctionField[A, B]): RationalFunction[A, B] = field.pow(this, exp)

  def ===(other: RationalFunction[A, B])(implicit field: RationalFunctionField[A, B]): Boolean = field.areEqual(this, other)
}
