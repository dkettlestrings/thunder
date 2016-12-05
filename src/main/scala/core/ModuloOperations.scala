package core

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

import language.implicitConversions

/**
  * Adds modding operations to EuclideanRings.
  */
object ModuloOperations {

  implicit def toModdable[A](er: EuclideanRing[A]): ModdableEuclideanRing[A] = new ModdableEuclideanRing[A] {

    override def domain = er
  }

  trait ModdableEuclideanRing[A] {

    implicit def domain: EuclideanRing[A]

    /**
      * Mods the EuclideanRing by the element a and returns a CommutativeRing.
      *
      * Use this operation when modding out by a reducible (non-irreducible) element.
      * @param a
      * @return
      */
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

    //TODO: reuse code above like in PolynomialRingOps
    /**
      * Mods the EuclideanRing by the element a and returns a Field.
      *
      * Use this operation when modding out by an irreducible element.  For performance reasons, this operation does
      * not check that criterion, it is up to the user to ensure this requirement.  If this requirement is not met,
      * you may get runtime errors.
      * @param a
      * @param context
      * @return
      */
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
