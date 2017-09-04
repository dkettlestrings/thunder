package polynomial

import algebra.ring.CommutativeRing
import core.InfixOps._

private [polynomial] trait PolynomialRingOps[A] {

  implicit def coefficientRing: CommutativeRing[A]

  implicit def param: FormalParameter

  def zero: Polynomial[A] = Polynomial[A](param, coefficientRing.zero)

  def one: Polynomial[A] = Polynomial[A](param, coefficientRing.one)

  def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = (x, y) match {

    case (x, z) if z == zero => x
    case (z, y) if z == zero => y
    case _ =>

      val sumsOfCoefficients = for {

        d <- 0 to List(x.degree.toInt, y.degree.toInt).max
        c1 = x.coefficient(d)
        c2 = y.coefficient(d)
      } yield c1.getOrElse(coefficientRing.zero) + c2.getOrElse(coefficientRing.zero)

      Polynomial(param, sumsOfCoefficients.toList.reverse)

  }

  def negate(x: Polynomial[A]): Polynomial[A] = x match {

    case z if z == zero => zero
    case _ =>
      val negatedCoefficients = for {

        d <- 0 to x.degree.toInt
        coefficient = x.coefficient(d)
      } yield coefficient.get.negate

      Polynomial(param, negatedCoefficients.toList.reverse)

  }

  def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = (x, y) match {

    case (_, z) if z == zero => zero
    case (z, _) if z == zero => zero
    case _ =>

      val terms = for {

        d1 <- 0 to x.degree.toInt
        d2 <- 0 to y.degree.toInt

        c1 = x.coefficient(d1)
        c2 = y.coefficient(d2)
      } yield Polynomial[A](param, c1.get * c2.get :: List.fill(d1 + d2)(coefficientRing.zero))

      terms.foldLeft(zero)(plus)

  }
}
