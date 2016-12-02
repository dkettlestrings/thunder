package core

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

import language.implicitConversions

object ModuloOperations {

  implicit def toModdable[A](er: EuclideanRing[A]): ModdableEuclideanRing[A] = new ModdableEuclideanRing[A] {

    override def domain = er
  }

  trait ModdableEuclideanRing[A] {

    implicit def domain: EuclideanRing[A]

    def modulo_r(a: A): CommutativeRing[ResidueClass[A]] = new CommutativeRing[ResidueClass[A]] {

      override def one: ResidueClass[A] = ResidueClass(domain.one, a)

      override def times(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
        require(x.modulus == y.modulus)
        ResidueClass(domain.times(x.representative, y.representative), a)
      }

      override def negate(x: ResidueClass[A]): ResidueClass[A] = ResidueClass(domain.negate(x.representative), a)

      override def zero: ResidueClass[A] = ResidueClass(domain.zero, a)

      override def plus(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
        require(x.modulus == y.modulus)
        ResidueClass(domain.plus(x.representative, y.representative), a)
      }
    }

    //TODO: reuse code above
    def modulo_f(a: A)(implicit context: EuclideanRingModdingContext[A]): Field[ResidueClass[A]] = new Field[ResidueClass[A]] {

      override def one: ResidueClass[A] = ResidueClass(domain.one, a)

      override def times(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
        require(x.modulus == y.modulus)
        ResidueClass(domain.times(x.representative, y.representative), a)
      }

      override def negate(x: ResidueClass[A]): ResidueClass[A] = ResidueClass(domain.negate(x.representative), a)

      override def zero: ResidueClass[A] = ResidueClass(domain.zero, a)

      override def plus(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
        require(x.modulus == y.modulus)
        ResidueClass(domain.plus(x.representative, y.representative), a)
      }

      override def div(x: ResidueClass[A], y: ResidueClass[A]): ResidueClass[A] = {
        require(x.modulus == y.modulus)
        require(y != this.zero)
        val possibleInverses = context.elementsUpTo(y.modulus)
        val inverseOption = possibleInverses.find(elem => this.times(ResidueClass[A](y.representative, y.modulus), ResidueClass[A](elem, y.modulus)) == this.one)
        val yInverse: ResidueClass[A] = inverseOption match {
          case Some(e) => ResidueClass(e, y.modulus)
          case None => throw new RuntimeException(s"Could not invert $y with modulus ${y.modulus}.  Are you sure the modulus is irreducible?")
        }
        this.times(x, yInverse)
      }

      //TODO: update algebra dependency so I don't have to override this stuff
      override def mod(a: ResidueClass[A], b: ResidueClass[A]): ResidueClass[A] = ???
      override def quot(a: ResidueClass[A], b: ResidueClass[A]): ResidueClass[A] = ???
    }
  }

}
