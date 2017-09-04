package core

import EuclideanDomainOps._

private[core] trait ModuloRingOps[A] {

  implicit def domain: EuclideanDomain[A]

  def modulus: A

  def zero: ResidueClass[A] = ResidueClass(domain.zero, modulus)

  def one: ResidueClass[A] = ResidueClass(domain.one, modulus)

  def plus(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
    require(x.modulus == y.modulus)
    ResidueClass(x.representative + y.representative, modulus)
  }

  def negate(x: ResidueClass[A]): ResidueClass[A] = ResidueClass(x.representative.negate, modulus)

  def times(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
    require(x.modulus == y.modulus)
    ResidueClass(x.representative * y.representative, modulus)
  }
}
