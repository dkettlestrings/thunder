package core

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

import language.implicitConversions

/**
  * Adds modding operations to EuclideanRings.
  */
object ModuloOperations {

  implicit def toModdable[A](er: EuclideanRing[A]): ModdableEuclideanRing[A] = new ModdableEuclideanRing[A] {

    override def originalRing = er
  }

  trait ModdableEuclideanRing[A] {

    implicit def originalRing: EuclideanRing[A]

    /**
      * Mods the EuclideanRing by the element a and returns a CommutativeRing.
      *
      * Use this operation when modding out by a reducible (non-irreducible) element.
      * @param m
      * @return
      */
    def modulo_r(m: A): CommutativeRing[ResidueClass[A]] = new CommutativeRing[ResidueClass[A]] with ModuloRingOps[A] {

      override def domain: EuclideanRing[A] = originalRing

      override def modulus: A = m

    }

    /**
      * Mods the EuclideanRing by the element a and returns a Field.
      *
      * Use this operation when modding out by an irreducible element.  For performance reasons, this operation does
      * not check that criterion, it is up to the user to ensure this requirement.  If this requirement is not met,
      * you may get runtime errors.
      * @param m
      * @param context
      * @return
      */
    def modulo_f(m: A)(implicit context: EuclideanRingModdingContext[A]): Field[ResidueClass[A]] = new Field[ResidueClass[A]] with ModuloRingOps[A] {

      override def domain: EuclideanRing[A] = originalRing

      override def modulus: A = m

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
