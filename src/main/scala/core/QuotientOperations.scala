package core

import EuclideanDomainOps._
import scala.language.implicitConversions

/**
  * Adds the operation of creating a quotient Field from a EuclideanDomain.
  */
object QuotientOperations {

  implicit def toRationalizable[A](er: EuclideanDomain[A]): RationalizableEuclideanDomain[A] = new RationalizableEuclideanDomain[A] {

    override def domain: EuclideanDomain[A] = er
  }

  trait RationalizableEuclideanDomain[A] {

    implicit def domain: EuclideanDomain[A]

    /**
      * Creates the field of [[RationalExpression]]s (quotients) of elements of the EuclideanDomain.
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
        val newNum = (x.numerator * y.denominator) + (x.denominator * y.numerator)
        RationalExpression(newNum, newDenom)
      }

      override def negate(x: RationalExpression[A]): RationalExpression[A] = {
        RationalExpression(x.numerator.negate, x.denominator)
      }

      override def times(x: RationalExpression[A], y: RationalExpression[A]): RationalExpression[A] = {
        val newNum = x.numerator * y.numerator
        val newDenom = x.denominator * y.denominator
        RationalExpression(newNum, newDenom)
      }

      override def div(x: RationalExpression[A], y: RationalExpression[A]): RationalExpression[A] = {

        if(y == zero) throw new ArithmeticException("Cannot divide by zero!")
        val newNum = x.numerator * y.denominator
        val newDenom = x.denominator * y.numerator
        RationalExpression(newNum, newDenom)
      }

    }

  }

}
