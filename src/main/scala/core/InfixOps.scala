package core

import algebra.ring.CommutativeRing

import scala.language.implicitConversions

object FieldOps {

  implicit def toInfix[A](value: A)(implicit ev: Field[A]): FieldOps[A] = new FieldOps[A] {
    override val a: A = value
    override val field: Field[A] = ev
  }

  trait FieldOps[A] {

    val a: A

    val field: Field[A]

    def +(other: A): A = field.plus(a, other)

    def -(other: A): A = field.minus(a, other)

    def *(other: A): A = field.times(a, other)

    def ^(exp: Int): A = field.pow(a, exp)

    def negate: A = field.negate(a)

    def /(other: A): A = field.div(a, other)

    def inv: A = field.div(field.one, a)

  }
}

object EuclideanDomainOps {

  implicit def toInfix[A](value: A)(implicit ev: EuclideanDomain[A]): EuclideanRingOps[A]  = new EuclideanRingOps[A] {
    override val a = value
    override val domain = ev
  }

  trait EuclideanRingOps[A] {

    val a: A

    val domain: EuclideanDomain[A]

    def +(other: A): A = domain.plus(a, other)

    def -(other: A): A = domain.minus(a, other)

    def *(other: A): A = domain.times(a, other)

    def ^(exp: Int): A = domain.pow(a, exp)

    def negate: A = domain.negate(a)

    def mod(other: A): A = domain.mod(a, other)

    def quot(other: A): A = domain.quot(a, other)

    def norm: ExtendedInteger = domain.norm(a)

  }
}

object CommutativeRingOps {

  implicit def toInfix[A](value: A)(implicit ev: CommutativeRing[A]): CommutativeRingOps[A] = new CommutativeRingOps[A] {
    override val a = value
    override val ring = ev
  }

  trait CommutativeRingOps[A] {

    val a: A

    val ring: CommutativeRing[A]

    def +(other: A): A = ring.plus(a, other)

    def -(other: A): A = ring.minus(a, other)

    def *(other: A): A = ring.times(a, other)

    def ^(exp: Int): A = ring.pow(a, exp)

    def negate: A = ring.negate(a)
  }
}
