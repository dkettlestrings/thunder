package finitefield

import core.{ExtendedInteger, ResidueClass}
import integer.IntegersModP
import polynomial.{FormalParameter, Polynomial, PolynomialRingOverField}
import core.EuclideanDomainOps._

trait PolynomialRingOverIntegersModP extends PolynomialRingOverField[ResidueClass[Int]] {

  override def coefficients: IntegersModP

}

//TODO: Is there a better way than using this delegate?  Inheritance, typeclass, macro, etc?  See https://github.com/dkettlestrings/thunder/issues/62
object PolynomialRingOverIntegersModP {

  def apply[A](p: Int, param: FormalParameter): PolynomialRingOverIntegersModP = {

    val fieldOfCoefficients = IntegersModP(p)

    implicit val delegate = PolynomialRingOverField(fieldOfCoefficients, param)

    new PolynomialRingOverIntegersModP {

      override def coefficients: IntegersModP = fieldOfCoefficients

      override def parameter: FormalParameter = param

      override def zero: Polynomial[ResidueClass[Int]] = delegate.zero

      override def one: Polynomial[ResidueClass[Int]] = delegate.one

      override def plus(x: Polynomial[ResidueClass[Int]], y: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = x + y

      override def negate(x: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = x.negate

      override def times(x: Polynomial[ResidueClass[Int]], y: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = x * y

      override def quot(a: Polynomial[ResidueClass[Int]], b: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = a quot b

      override def mod(a: Polynomial[ResidueClass[Int]], b: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = a mod b

      override def norm(a: Polynomial[ResidueClass[Int]]): ExtendedInteger = a.norm
    }


  }
}
