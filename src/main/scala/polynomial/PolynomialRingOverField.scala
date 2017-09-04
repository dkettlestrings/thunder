package polynomial

import core.{EuclideanDomain, ExtendedInteger, Field}
import polynomial.AdjoiningOperations._
import core.InfixOps._

trait PolynomialRingOverField[A] extends EuclideanDomain[Polynomial[A]] with PolynomialRing[A] {

  def parameter: FormalParameter

  def coefficients: Field[A]

  private implicit val delegate = coefficients f_adjoin parameter

  override def quot(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = a quot b

  override def mod(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = a mod b

  override def norm(a: Polynomial[A]): ExtendedInteger = a.norm

}

object PolynomialRingOverField {

  def apply[A](coefficientField: Field[A], p: FormalParameter): PolynomialRingOverField[A] = new PolynomialRingOverField[A] {

    override def parameter: FormalParameter = p

    override def coefficients: Field[A] = coefficientField
  }
}
