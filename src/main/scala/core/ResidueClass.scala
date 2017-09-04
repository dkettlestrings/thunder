package core

/**
  *
  * @tparam A
  */
trait ResidueClass[A] extends EqualityShim[ResidueClass[A]] with ArithmeticOps[ResidueClass[A]] {

  override val me: ResidueClass[A] = this

  def domain: EuclideanDomain[A]

  def representative: A

  def modulus: A

  override def equalz(other: ResidueClass[A]): Boolean = {

    this.modulus == other.modulus &&
    domain.mod(representative, modulus) == domain.mod(other.representative, other.modulus)
  }

  override def hashCode(): Int = modulus.hashCode * domain.mod(representative, modulus).hashCode

  override def toString: String = s"[$representative]_$modulus"

}

/**
  * Companion object for construction.
  */
object ResidueClass {

  def apply[A](residue: A, mod: A)(implicit dom: EuclideanDomain[A]): ResidueClass[A] = new ResidueClass[A] {

    override val domain: EuclideanDomain[A] = dom

    override val modulus: A = mod

    override val representative: A = domain.mod(residue, modulus)

  }
}
