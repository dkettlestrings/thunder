package functionfield

import core.ResidueClass
import finitefield.FiniteField
import polynomial.{FormalParameter, Polynomial, PolynomialRingOverField}

trait PolynomialRingOverFiniteField extends PolynomialRingOverField[ResidueClass[Polynomial[ResidueClass[Int]]]]{

}

object PolynomialRingOverFiniteField {

  def apply(field: FiniteField, p: FormalParameter): PolynomialRingOverFiniteField = new PolynomialRingOverFiniteField {

    override val coefficients = field

    override val parameter = p

  }
}
