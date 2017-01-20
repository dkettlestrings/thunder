package polynomial

import AdjoiningOperations._
import algebra.ring.CommutativeRing

trait PolynomialRing[A] extends CommutativeRing[Polynomial[A]] {

  def parameter: FormalParameter

  def coefficients: CommutativeRing[A]

}

object PolynomialRing {

  def apply[A](coefficientRing: CommutativeRing[A], p: FormalParameter): PolynomialRing[A] = new PolynomialRing[A] {

    override def coefficients: CommutativeRing[A] = coefficientRing

    override def parameter: FormalParameter = p

    private val delegate = coefficients r_adjoin p

    override def zero: Polynomial[A] = delegate.zero

    override def one: Polynomial[A] = delegate.one

    override def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = delegate.plus(x, y)

    override def negate(x: Polynomial[A]): Polynomial[A] = delegate.negate(x)

    override def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = delegate.times(x, y)

  }
}
