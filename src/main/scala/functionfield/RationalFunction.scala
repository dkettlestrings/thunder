package functionfield

import core.{EuclideanDomain, RationalExpression}
import polynomial.{Polynomial, PolynomialRingOverField}

trait RationalFunction[A] extends RationalExpression[Polynomial[A]] {

  def polyRing: PolynomialRingOverField[A]

}

object RationalFunction {

  def apply[A](num: Polynomial[A], denom: Polynomial[A])(implicit ring: PolynomialRingOverField[A]): RationalFunction[A] =
    new RationalFunction[A] {

      override def polyRing: PolynomialRingOverField[A] = ring

      override def denominator: Polynomial[A] = denom

      override def numerator: Polynomial[A] = num

      override implicit def domain: EuclideanDomain[Polynomial[A]] = ring
    }

  implicit def toRationalFunction[A](r: RationalExpression[Polynomial[A]])(implicit ring: PolynomialRingOverField[A]): RationalFunction[A] = {

    apply(r.numerator, r.denominator)(ring)
  }
}
