package core

import algebra.ring.CommutativeRing

import scala.util.{Failure, Success, Try}

/**
  * A quotient of elements from a EuclideanDomain.
  *
  * Think of it as a fraction.
  *
  * @tparam A
  */
trait RationalExpression[A] extends EquivalenceClass[(A, A)] {

  def domain: EuclideanDomain[A]

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

  def +(that: RationalExpression[A])(implicit ring: CommutativeRing[RationalExpression[A]]): RationalExpression[A] = ring.plus(this, that)

  def -(that: RationalExpression[A])(implicit ring: CommutativeRing[RationalExpression[A]]): RationalExpression[A] = ring.minus(this, that)

  def *(that: RationalExpression[A])(implicit ring: CommutativeRing[RationalExpression[A]]): RationalExpression[A] = ring.times(this, that)

  /**
    * Exponentiation (repeated multiplication) operator.
    *
    * Note that this implementation adopts the convention of if z is the zero element, z^0 == 0.
    *
    * @param exp
    * @param ring
    * @return
    */
  def ^(exp: Int)(implicit ring: CommutativeRing[RationalExpression[A]]): RationalExpression[A] = ring.pow(this, exp)

  def negate(implicit ring: CommutativeRing[RationalExpression[A]]): RationalExpression[A] = ring.negate(this)

  def /(that: RationalExpression[A])(implicit field: Field[RationalExpression[A]]): RationalExpression[A] = field.div(this, that)

  override def representative: (A, A) = (numerator, denominator)

  override def relation: EquivalenceRelation[(A, A)] = new EquivalenceRelation[(A, A)] {

    override def areEquivalent(x: (A, A), y: (A, A)): Boolean = {

      val (num1, denom1) = x
      val (num2, denom2) = y

      domain.times(num1, denom2) == domain.times(denom1, num2)
    }
  }

  //TODO: Create some kind of Equalable trait to wrap the type checking.  This would be used in many places.  See https://github.com/dkettlestrings/thunder/issues/46
  override def equals(obj: scala.Any): Boolean = {

    Try(obj.asInstanceOf[RationalExpression[A]]) match {
      case Success(re) => this.relation.areEquivalent(this.representative, re.representative)
      case Failure(throwable) => false
    }
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
