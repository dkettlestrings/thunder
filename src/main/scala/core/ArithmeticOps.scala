package core

import algebra.ring.CommutativeRing

/**
  *
  * @tparam A
  */
trait ArithmeticOps[A] {

  // TODO: This me field is a hack that requires users to override it.  Figure out a way for clients to not have to do this. See https://github.com/dkettlestrings/thunder/issues/58
  def me: A

  def +(other: A)(implicit ring: CommutativeRing[A]): A = ring.plus(me, other)

  def -(other: A)(implicit ring: CommutativeRing[A]): A = ring.minus(me, other)

  def *(other: A)(implicit ring: CommutativeRing[A]): A = ring.times(me, other)

  def ^(exp: Int)(implicit ring: CommutativeRing[A]): A = ring.pow(me, exp)

  def negate(implicit ring: CommutativeRing[A]): A = ring.negate(me)

  def /(other: A)(implicit field: Field[A]): A = field.div(me, other)

  def inv(implicit field: Field[A]): A = field.div(field.one, me)

}
