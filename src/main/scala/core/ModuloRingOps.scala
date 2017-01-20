package core

import algebra.ring.EuclideanRing

private[core] trait ModuloRingOps[A] {

  implicit def domain: EuclideanRing[A]

  def modulus: A

  def zero: ResidueClass[A] = ResidueClass(domain.zero, modulus)

  def one: ResidueClass[A] = ResidueClass(domain.one, modulus)

  def plus(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
    require(x.modulus == y.modulus)
    ResidueClass(domain.plus(x.representative, y.representative), modulus)
  }

  def negate(x: ResidueClass[A]): ResidueClass[A] = ResidueClass(domain.negate(x.representative), modulus)

  def times(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
    require(x.modulus == y.modulus)
    ResidueClass(domain.times(x.representative, y.representative), modulus)
  }
}
