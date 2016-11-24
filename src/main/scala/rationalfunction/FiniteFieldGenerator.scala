package rationalfunction

import finitefields.{PrimeField, ResidueClass}
import polynomial.{FormalParameter, Polynomial1, Polynomial1RingOverField}
import polynomial.Predef.X

object FiniteFieldGenerator {

  def extend(field: PrimeField, exponent: Int): FunctionField[ResidueClass, PrimeField] = {

    implicit def polyRing = Polynomial1RingOverField[ResidueClass, PrimeField](X, field)

    val one = polyRing.coefficientField.one
    val zero = polyRing.coefficientField.zero

    //now define the polynomial equation which is X^n - X = 0
    val relation: Polynomial1[ResidueClass] = (polyRing.polynomial(one, zero) ^ exponent) - polyRing.polynomial(one, zero)

    new FunctionField[ResidueClass, PrimeField] {

      override def param: FormalParameter = X

      override implicit def fieldOfConstants: PrimeField = field

      def reduce(p: Polynomial1[ResidueClass]) = polynomialRing.mod(p, relation)

      override def rationalFunction(num: Polynomial1[ResidueClass], denom: Polynomial1[ResidueClass]): RationalFunction[ResidueClass, PrimeField] = {
        val newNum = reduce(num)
        val newDenom = reduce(denom)
        if(newDenom == polynomialRing.zero) throw new ArithmeticException("Dividing by zero")
        RationalFunction[ResidueClass, PrimeField](polynomialRing.param, newNum, newDenom)
      }


    }

  }

}
