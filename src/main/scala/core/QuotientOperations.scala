package core

import algebra.ring.{EuclideanRing, Field}

import language.implicitConversions

/**
  * Adds the operation of creating a quotient field from a EuclideanRing.
  */
object QuotientOperations {

  implicit def toRationalizable[A](er: EuclideanRing[A]): RationalizeAbleEuclideanRing[A] = new RationalizeAbleEuclideanRing[A] {

    override def domain: EuclideanRing[A] = er
  }

  trait RationalizeAbleEuclideanRing[A] {

    implicit def domain: EuclideanRing[A]

    /**
      * Creates the field of RationalExpressions (quotients) of elements of the EuclideanRing.
      * @return
      */
    def quotientField: Field[RationalExpression[A, EuclideanRing[A]]] = new Field[RationalExpression[A, EuclideanRing[A]]] {

      override def zero: RationalExpression[A, EuclideanRing[A]] = RationalExpression(domain.zero, domain.one)

      override def one: RationalExpression[A, EuclideanRing[A]] = RationalExpression(domain.one, domain.one)

      override def plus(x: RationalExpression[A, EuclideanRing[A]], y: RationalExpression[A, EuclideanRing[A]]): RationalExpression[A, EuclideanRing[A]] = {

        val newDenom = domain.times(x.denominator, y.denominator)
        val newNum = domain.plus(domain.times(x.numerator, y.denominator), domain.times(x.denominator, y.numerator))
        RationalExpression(newNum, newDenom)
      }

      override def negate(x: RationalExpression[A, EuclideanRing[A]]): RationalExpression[A, EuclideanRing[A]] = {
        RationalExpression(domain.negate(x.numerator), x.denominator)
      }

      override def times(x: RationalExpression[A, EuclideanRing[A]], y: RationalExpression[A, EuclideanRing[A]]): RationalExpression[A, EuclideanRing[A]] = {
        val newNum = domain.times(x.numerator, y.numerator)
        val newDenom = domain.times(x.denominator, y.denominator)
        RationalExpression(newNum, newDenom)
      }

      override def div(x: RationalExpression[A, EuclideanRing[A]], y: RationalExpression[A, EuclideanRing[A]]): RationalExpression[A, EuclideanRing[A]] = {

        if(y == zero) throw new ArithmeticException("Cannot divide by zero!")
        val newNum = domain.times(x.numerator, y.denominator)
        val newDenom = domain.times(x.denominator, y.numerator)
        RationalExpression(newNum, newDenom)
      }

      //TODO: update algebra dependency so I can use default implementations
      override def quot(a: RationalExpression[A, EuclideanRing[A]], b: RationalExpression[A, EuclideanRing[A]]): RationalExpression[A, EuclideanRing[A]] = ???

      override def mod(a: RationalExpression[A, EuclideanRing[A]], b: RationalExpression[A, EuclideanRing[A]]): RationalExpression[A, EuclideanRing[A]] = ???
    }

  }

}
