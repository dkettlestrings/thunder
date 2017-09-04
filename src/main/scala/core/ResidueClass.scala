package core

import scala.util.{Try, Success, Failure}

/**
  * An equivalence class of elements from a EuclideanDomain produced by modding out by an element.
  *
  * A ResidueClass is a specialized [[EquivalenceClass]] for capturing the result of modding a
  * EuclideanDomain by an element.  An important note here is that due to this extra constraint, we can
  * get better equality than in the basic [[EquivalenceClass]].  For a basic [[EquivalenceClass]], there is
  * no ability to check whether the [[EquivalenceRelation]]s are the same, but for ResidueClasses, we can simply
  * check the modulus.
  *
  * @tparam A
  */
trait ResidueClass[A] extends EquivalenceClass[A] with ArithmeticOps[ResidueClass[A]] {

  override val me = this

  def modulus: A

  def inv(implicit field: Field[ResidueClass[A]]): ResidueClass[A] = field.div(field.one, this)

  //TODO: Create some kind of Equalable trait to wrap the type checking.  This would be used in many places.  See https://github.com/dkettlestrings/thunder/issues/46
  override def equals(obj: scala.Any): Boolean = {

    Try(obj.asInstanceOf[ResidueClass[A]]) match {
      case Success(rc) => this.modulus == rc.modulus && this.contains(rc.representative)
      case Failure(throwable) => false
    }
  }

  override def hashCode(): Int = modulus.hashCode() + representative.hashCode()

  override def toString: String = s"[$representative]_$modulus"

}

/**
  * Companion object for construction.
  */
object ResidueClass {

  def apply[A](residue: A, mod: A)(implicit domain: EuclideanDomain[A]): ResidueClass[A] = new ResidueClass[A] {

    override val modulus: A = mod

    override val representative: A = domain.mod(residue, modulus)

    override def relation: EquivalenceRelation[A] = EquivalenceRelation[A, A](x => domain.mod(x, modulus))
  }
}
