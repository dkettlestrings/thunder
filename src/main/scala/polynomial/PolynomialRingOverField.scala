package polynomial

import algebra.ring.{EuclideanRing, Field}
import polynomial.AdjoiningOperations._

trait PolynomialRingOverField[A] extends EuclideanRing[Polynomial[A]] {

  def parameter: FormalParameter

  def coefficients: Field[A]

}

object PolynomialRingOverField {

  def apply[A](coefficientField: Field[A], p: FormalParameter): PolynomialRingOverField[A] = new PolynomialRingOverField[A] {

    override def parameter: FormalParameter = p

    override def coefficients: Field[A] = coefficientField

    private val delegate = coefficients f_adjoin p

    override def zero: Polynomial[A] = delegate.zero

    override def one: Polynomial[A] = delegate.one

    override def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = delegate.plus(x, y)

    override def negate(x: Polynomial[A]): Polynomial[A] = delegate.negate(x)

    override def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = delegate.times(x, y)

    override def quot(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = delegate.quot(a, b)

    override def mod(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = delegate.mod(a, b)
  }
}
