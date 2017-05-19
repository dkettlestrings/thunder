package finitefield

import core.ResidueClass
import integer.IntegersModP
import polynomial.{FormalParameter, Polynomial, PolynomialRingOverField}

trait PolynomialRingOverIntegersModP extends PolynomialRingOverField[ResidueClass[Int]] {

  override def coefficients: IntegersModP

}

object PolynomialRingOverIntegersModP {

  def apply[A](p: Int, param: FormalParameter): PolynomialRingOverIntegersModP = {

    val fieldOfCoefficients = IntegersModP(p)

    val delegate = PolynomialRingOverField(fieldOfCoefficients, param)

    new PolynomialRingOverIntegersModP {

      override def coefficients: IntegersModP = fieldOfCoefficients

      override def parameter: FormalParameter = param

      override def zero: Polynomial[ResidueClass[Int]] = delegate.zero

      override def one: Polynomial[ResidueClass[Int]] = delegate.one

      override def plus(x: Polynomial[ResidueClass[Int]], y: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = delegate.plus(x, y)

      override def negate(x: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = delegate.negate(x)

      override def times(x: Polynomial[ResidueClass[Int]], y: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = delegate.times(x, y)

      override def quot(a: Polynomial[ResidueClass[Int]], b: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = ???

      override def mod(a: Polynomial[ResidueClass[Int]], b: Polynomial[ResidueClass[Int]]): Polynomial[ResidueClass[Int]] = ???
    }


  }
}
