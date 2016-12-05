package core

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

//TODO: override equals since we can do "function equality" by inspecting the modulus.
/**
  * An equivalence class of elements from a EuclideanRing produced by modding out by an element.
  *
  * A ResidueClass is a specialized EquivalenceClass for capturing the result of modding a EuclideanRing by an element.
  * An important note here is that due to this extra constraint, we can get better equality than in the basic
  * EquivalenceClass.  For a basic EquivalenceClass, there is no ability to check whether the EquivalenceRelations are
  * the same, but for ResidueClasses, we can simply check the modulus.
  * @tparam A
  */
trait ResidueClass[A] extends EquivalenceClass[A] {

  def modulus: A

  def +(other: ResidueClass[A])(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.plus(this, other)

  def -(other: ResidueClass[A])(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.minus(this, other)

  def *(other: ResidueClass[A])(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.times(this, other)

  def ^(exp: Int)(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.pow(this, exp)

  def negate(implicit ring: CommutativeRing[ResidueClass[A]]): ResidueClass[A] = ring.negate(this)

  def /(other: ResidueClass[A])(implicit field: Field[ResidueClass[A]]): ResidueClass[A] = field.div(this, other)

  def inv(implicit field: Field[ResidueClass[A]]): ResidueClass[A] = field.div(field.one, this)

}

object ResidueClass {

  def apply[A](residue: A, mod: A)(implicit domain: EuclideanRing[A]): ResidueClass[A] = new ResidueClass[A] {

    override def modulus: A = mod

    override def representative: A = residue

    override def relation: EquivalenceRelation[A] = EquivalenceRelation[A, A](x => domain.mod(x, modulus))
  }
}
