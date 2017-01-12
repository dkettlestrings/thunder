package core

import algebra.ring.{CommutativeRing, EuclideanRing, Field}
import scala.util.{Try, Success, Failure}

/**
  * An equivalence class of elements from a EuclideanRing produced by modding out by an element.
  *
  * A ResidueClass is a specialized [[EquivalenceClass]] for capturing the result of modding a
  * EuclideanRing by an element.  An important note here is that due to this extra constraint, we can
  * get better equality than in the basic [[EquivalenceClass]].  For a basic [[EquivalenceClass]], there is
  * no ability to check whether the [[EquivalenceRelation]]s are the same, but for ResidueClasses, we can simply
  * check the modulus.
  *
  * @tparam A
  */
trait ResidueClass[A] extends EquivalenceClass[A] {

  def modulus: A

  def +(other: ResidueClass[A])(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.plus(this, other)

  def -(other: ResidueClass[A])(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.minus(this, other)

  def *(other: ResidueClass[A])(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.times(this, other)

  /**
    * Exponentiation (repeated multiplication) operator.
    *
    * Note that this implementation adopts the convention of if z is the zero element, z^0 == 0.
    *
    * @param exp
    * @param ring
    * @return
    */
  def ^(exp: Int)(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.pow(this, exp)

  def negate(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.negate(this)

  def /(other: ResidueClass[A])(implicit field: Field[ResidueClass[A]]): ResidueClass[A] = field.div(this, other)

  def inv(implicit field: Field[ResidueClass[A]]): ResidueClass[A] = field.div(field.one, this)

  //TODO: Create some kind of Equalable trait to wrap the type checking.  This would be used in many places.  See https://github.com/dkettlestrings/thunder/issues/46
  override def equals(obj: scala.Any): Boolean = {

    Try(obj.asInstanceOf[ResidueClass[A]]) match {
      case Success(rc) => this.modulus == rc.modulus && this.contains(rc.representative)
      case Failure(throwable) => false
    }
  }

  override def toString: String = s"[$representative]_$modulus"

}

/**
  * Companion object for construction.
  */
object ResidueClass {

  def apply[A](residue: A, mod: A)(implicit domain: EuclideanRing[A]): ResidueClass[A] = new ResidueClass[A] {

    override def modulus: A = mod

    override def representative: A = residue

    override def relation: EquivalenceRelation[A] = EquivalenceRelation[A, A](x => domain.mod(x, modulus))
  }
}
