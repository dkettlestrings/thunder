package core

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

trait RationalExpression[A, B <: EuclideanRing[A]] extends EquivalenceClass[(A, A)] {

  def domain: B

  def numerator: A

  def denominator: A

  def ===(that: RationalExpression[A, B]): Boolean = this.numerator == that.numerator && this.denominator == that.denominator

  def !==(that: RationalExpression[A, B]): Boolean = !(this === that)

  def +(that: RationalExpression[A, B])(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.plus(this, that)

  def -(that: RationalExpression[A, B])(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.minus(this, that)

  def *(that: RationalExpression[A, B])(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.times(this, that)

  def ^(exp: Int)(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.pow(this, exp)

  def negate(implicit ring: CommutativeRing[RationalExpression[A, B]]): RationalExpression[A, B] = ring.negate(this)

  def /(that: RationalExpression[A, B])(implicit field: Field[RationalExpression[A, B]]): RationalExpression[A, B] = field.div(this, that)

  override def representative: (A, A) = (numerator, denominator)

  override def relation: EquivalenceRelation[(A, A)] = new EquivalenceRelation[(A, A)] {

    override def areEquivalent(x: (A, A), y: (A, A)): Boolean = {

      val (num1, denom1) = x
      val (num2, denom2) = y

      domain.times(num1, denom2) == domain.times(denom1, num2)
    }
  }
}

object RationalExpression {

  def apply[A, B <: EuclideanRing[A]](num: A, denom: A)(implicit euclideanRing: B): RationalExpression[A, B] = new RationalExpression[A, B] {

    override def domain: B = euclideanRing

    override def numerator: A = num

    override def denominator: A = denom
  }
}
