package polynomial

import algebra.ring.{CommutativeRing, Field}

object AdjoiningOperations {

  implicit def toAdjoinableCommutativeRing[A](r: CommutativeRing[A]): AdjoinableCommutativeRing[A] = new AdjoinableCommutativeRing[A] {
    override def domain: CommutativeRing[A] = r
  }

  implicit def toAdjoinableField[A](f: Field[A]): AdjoinableField[A] = new AdjoinableField[A] {
    override def field: Field[A] = f
  }

  trait AdjoinableCommutativeRing[A] {

    def domain: CommutativeRing[A]

    def adjoin(x: FormalParameter): PolynomialRing[A] = PolynomialRing(x, domain)
  }

  trait AdjoinableField[A] {

    def field: Field[A]

    def adjoin(x: FormalParameter): PolynomialRingOverField[A, Field[A]] = PolynomialRingOverField(x, field)
  }

}
