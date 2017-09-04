package core

import InfixOps._

/**
  *
  * @tparam A
  */
trait ResidueClass[A] extends EqualityShim[ResidueClass[A]]{

  implicit def domain: EuclideanDomain[A]

  def representative: A

  def modulus: A

  override def equalz(other: ResidueClass[A]): Boolean = {

    this.modulus == other.modulus &&
      (representative mod modulus) == (other.representative mod other.modulus)
  }

  override def hashCode(): Int = modulus.hashCode * (representative mod modulus).hashCode

  override def toString: String = s"[$representative]_$modulus"

}

/**
  * Companion object for construction.
  */
object ResidueClass {

  def apply[A](residue: A, mod: A)(implicit dom: EuclideanDomain[A]): ResidueClass[A] = new ResidueClass[A] {

    override val domain: EuclideanDomain[A] = dom

    override val modulus: A = mod

    override val representative: A = residue mod modulus

  }
}
