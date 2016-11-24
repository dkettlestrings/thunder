package rationalfunction

import algebra.ring.Field
import polynomial.{FormalParameter, Polynomial1, Polynomial1RingOverField}

object FFGenerator {

  def extend[A, B <: Field[A]](baseFunctionField: FunctionField[A, B], relation: Polynomial1[RationalFunction[A, B]]): FunctionField[RationalFunction[A, B], FunctionField[A, B]] = {

    implicit def polyRing = Polynomial1RingOverField[RationalFunction[A, B], FunctionField[A, B]](relation.param, baseFunctionField)

    new FunctionField[RationalFunction[A, B], FunctionField[A, B]] {

      override def param: FormalParameter = relation.param

      override implicit def fieldOfConstants = baseFunctionField

      def reduce(p: Polynomial1[RationalFunction[A, B]]) = polyRing.mod(p, relation)

      override def rationalFunction(num: Polynomial1[RationalFunction[A, B]], denom: Polynomial1[RationalFunction[A, B]]): RationalFunction[RationalFunction[A, B], FunctionField[A, B]] = {
        val newNum = reduce(num)
        val newDenom = reduce(denom)
        if(newDenom == polynomialRing.zero) throw new ArithmeticException("Dividing by zero")
        RationalFunction[RationalFunction[A, B], FunctionField[A, B]](polynomialRing.param, newNum, newDenom)
      }

    }
  }

}
