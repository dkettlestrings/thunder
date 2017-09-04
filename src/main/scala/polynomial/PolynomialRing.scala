package polynomial

import algebra.ring.CommutativeRing
import polynomial.AdjoiningOperations._
import core.InfixOps._

trait PolynomialRing[A] extends CommutativeRing[Polynomial[A]] {

  def parameter: FormalParameter

  def coefficients: CommutativeRing[A]

  private implicit val delegate = coefficients r_adjoin parameter

  override def zero: Polynomial[A] = delegate.zero

  override def one: Polynomial[A] = delegate.one

  override def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = x + y

  override def negate(x: Polynomial[A]): Polynomial[A] = x.negate

  override def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = x * y

}

object PolynomialRing {

  def apply[A](coefficientRing: CommutativeRing[A], p: FormalParameter): PolynomialRing[A] = new PolynomialRing[A] {

    override def coefficients: CommutativeRing[A] = coefficientRing

    override def parameter: FormalParameter = p
  }
}
