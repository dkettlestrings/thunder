package polynomial

import core.{EuclideanDomain, ExtendedInteger, Field}
import polynomial.AdjoiningOperations._
import core.EuclideanDomainOps._

trait PolynomialRingOverField[A] extends EuclideanDomain[Polynomial[A]] {

  def parameter: FormalParameter

  def coefficients: Field[A]

}

object PolynomialRingOverField {

  def apply[A](coefficientField: Field[A], p: FormalParameter): PolynomialRingOverField[A] = new PolynomialRingOverField[A] {

    override def parameter: FormalParameter = p

    override def coefficients: Field[A] = coefficientField

    private implicit val delegate = coefficients f_adjoin p

    override def zero: Polynomial[A] = delegate.zero

    override def one: Polynomial[A] = delegate.one

    override def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = x + y

    override def negate(x: Polynomial[A]): Polynomial[A] = x.negate

    override def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = x * y

    override def quot(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = a quot b

    override def mod(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = a mod b

    override def norm(a: Polynomial[A]): ExtendedInteger = a.norm
  }
}
