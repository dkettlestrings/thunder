package core

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

trait RationalExpression[A, B <: EuclideanRing[A]] {

  def domain: B

  def numerator: A

  def denominator: A

  def ==(that: RationalExpression[A, B]): Boolean = domain.times(this.numerator, that.denominator) == domain.times(this.denominator, that.numerator)

  def !=(that: RationalExpression[A, B]): Boolean = !(this == that)

  def ===(that: RationalExpression[A, B]): Boolean = this.numerator == that.numerator && this.denominator == that.denominator

  def !==(that: RationalExpression[A, B]): Boolean = !(this === that)

  def +(that: RationalExpression[A, B])(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.plus(this, that)

  def -(that: RationalExpression[A, B])(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.minus(this, that)

  def *(that: RationalExpression[A, B])(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.times(this, that)

  def ^(exp: Int)(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.pow(this, exp)

  def negate(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.negate(this)

  def /(that: RationalExpression[A, B])(implicit field: Field[RationalExpression[A, B]]): RationalExpression[A, B] = field.div(this, that)

}

object RationalExpression {

  def apply[A, B <: EuclideanRing[A]](num: A, denom: A)(implicit euclideanRing: B): RationalExpression[A, B] = new RationalExpression[A, B] {

    override def domain: B = euclideanRing

    override def numerator: A = num

    override def denominator: A = denom
  }
}
