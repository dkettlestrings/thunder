package core

import EuclideanDomainOps._

/**
  * A quotient of elements from a EuclideanDomain.
  *
  * Think of it as a fraction.
  *
  * @tparam A
  */
trait RationalExpression[A] extends EqualityShim[RationalExpression[A]] {

  implicit def domain: EuclideanDomain[A]

  def numerator: A

  def denominator: A

  /**
    * Checks that two RationalExpressions are representationally equivalent (same numerator and denomiator)
    *
    * If you want algebraic equility (they represent the same value in the field of quotients), use ==.
    * @param that
    * @return
    */
  def ===(that: RationalExpression[A]): Boolean = this.numerator == that.numerator && this.denominator == that.denominator

  def !==(that: RationalExpression[A]): Boolean = !(this === that)

  override def equalz(other: RationalExpression[A]): Boolean = {

    numerator * other.denominator == denominator * other.numerator
  }

  override def hashCode(): Int = numerator.hashCode() / denominator.hashCode()
}


/**
  * Companion object for construction.
  */
object RationalExpression {

  def apply[A](num: A, denom: A)(implicit euclideanRing: EuclideanDomain[A]): RationalExpression[A] = new RationalExpression[A] {

    override def domain: EuclideanDomain[A]= euclideanRing

    override def numerator: A = num

    override def denominator: A = denom
  }
}
