package core

import algebra.ring.{EuclideanRing, Field}

import language.implicitConversions

/**
  * Adds the operation of creating a quotient Field from a EuclideanRing.
  */
object QuotientOperations {

  implicit def toRationalizable[A](er: EuclideanRing[A]): RationalizeAbleEuclideanRing[A] = new RationalizeAbleEuclideanRing[A] {

    override def domain: EuclideanRing[A] = er
  }

  trait RationalizeAbleEuclideanRing[A] {

    implicit def domain: EuclideanRing[A]

    /**
      * Creates the field of [[RationalExpression]]s (quotients) of elements of the EuclideanRing.
      *
      * A good example of a field of quotients is the rational numbers.  You create "fractions" of elements of the
      * integers (a ring).
      * @return
      */
    def quotientField: Field[RationalExpression[A]] = new Field[RationalExpression[A]] {

      override def zero: RationalExpression[A] = RationalExpression(domain.zero, domain.one)

      override def one: RationalExpression[A] = RationalExpression(domain.one, domain.one)

      override def plus(x: RationalExpression[A], y: RationalExpression[A]): RationalExpression[A] = {

        val newDenom = domain.times(x.denominator, y.denominator)
        val newNum = domain.plus(domain.times(x.numerator, y.denominator), domain.times(x.denominator, y.numerator))
        RationalExpression(newNum, newDenom)
      }

      override def negate(x: RationalExpression[A]): RationalExpression[A] = {
        RationalExpression(domain.negate(x.numerator), x.denominator)
      }

      override def times(x: RationalExpression[A], y: RationalExpression[A]): RationalExpression[A] = {
        val newNum = domain.times(x.numerator, y.numerator)
        val newDenom = domain.times(x.denominator, y.denominator)
        RationalExpression(newNum, newDenom)
      }

      override def div(x: RationalExpression[A], y: RationalExpression[A]): RationalExpression[A] = {

        if(y == zero) throw new ArithmeticException("Cannot divide by zero!")
        val newNum = domain.times(x.numerator, y.denominator)
        val newDenom = domain.times(x.denominator, y.numerator)
        RationalExpression(newNum, newDenom)
      }

      //TODO: update algebra dependency so I can use default implementations see https://github.com/dkettlestrings/thunder/issues/15
      override def quot(a: RationalExpression[A], b: RationalExpression[A]): RationalExpression[A] = ???

      override def mod(a: RationalExpression[A], b: RationalExpression[A]): RationalExpression[A] = ???
    }

  }

}
