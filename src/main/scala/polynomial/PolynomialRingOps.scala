package polynomial

import algebra.ring.CommutativeRing

private [polynomial] trait PolynomialRingOps[A] {

  implicit def coefficientRing: CommutativeRing[A]

  implicit def param: FormalParameter

  def zero: Polynomial[A] = Polynomial[A](param, coefficientRing.zero)

  def one: Polynomial[A] = Polynomial[A](param, coefficientRing.one)

  def plus(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {

    if (x == zero) y
    else if (y == zero) x
    else {

      val sumsOfCoefficients = for {

        d <- 0 to List(x.degree.toInt, y.degree.toInt).max
        c1 = x.coefficient(d)
        c2 = y.coefficient(d)
      } yield coefficientRing.plus(c1.getOrElse(coefficientRing.zero), c2.getOrElse(coefficientRing.zero))

      Polynomial(param, sumsOfCoefficients.toList.reverse)

    }
  }

  def negate(x: Polynomial[A]): Polynomial[A] = {
    if (x == zero) zero
    else {
      val negatedCoefficients = for {

        d <- 0 to x.degree.toInt
        coefficient = x.coefficient(d)
      } yield coefficientRing.negate(coefficient.get)

      Polynomial(param, negatedCoefficients.toList.reverse)
    }
  }

  def times(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {

    if (x == zero || y == zero) zero
    else {

      val terms = for {

        d1 <- 0 to x.degree.toInt
        d2 <- 0 to y.degree.toInt

        c1 = x.coefficient(d1)
        c2 = y.coefficient(d2)
      } yield Polynomial[A](param, coefficientRing.times(c1.get, c2.get) :: List.fill(d1 + d2)(coefficientRing.zero))

      terms.foldLeft(zero)(plus)
    }
  }
}
