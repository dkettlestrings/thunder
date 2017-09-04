package finitefield

import core.ResidueClass
import integer.IntegersModP
import polynomial.{FormalParameter, PolynomialRingOverField}

trait PolynomialRingOverIntegersModP extends PolynomialRingOverField[ResidueClass[Int]] {

  override def coefficients: IntegersModP

}

object PolynomialRingOverIntegersModP {

  def apply[A](p: Int, param: FormalParameter): PolynomialRingOverIntegersModP = {

    new PolynomialRingOverIntegersModP {

      override def coefficients: IntegersModP = IntegersModP(p)

      override def parameter: FormalParameter = param
    }
  }
}
