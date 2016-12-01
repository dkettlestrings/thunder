package polynomial

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

import scala.annotation.tailrec

trait PolynomialRingOverField[A, B <: Field[A]] extends PolynomialRing[A] with EuclideanRing[Polynomial[A]] {

  implicit def coefficientField: B

  implicit def polyRing = this

  override implicit def coefficientRing: CommutativeRing[A] = coefficientField

  override def quot(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {

    @tailrec
    def go(a: Polynomial[A], b: Polynomial[A], acc: Polynomial[A]): Polynomial[A] = {
      if(a.degree < b.degree) acc
      else {
        val leadingCoefficient = coefficientField.div(a.leadingCoefficient, b.leadingCoefficient)
        val degreeDifference = a.degree - b.degree
        val xToTheN = polynomial(coefficientField.one, coefficientField.zero) ^ degreeDifference.toInt // X^(degreeDifference)
        val newQuot = xToTheN * polynomial(leadingCoefficient)
        val reducedA = a - (newQuot * b)
        go(reducedA, b, acc + newQuot)
      }
    }

    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else go(x, y, zero)

  }

  override def mod(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {
    x - (quot(x, y) * y)
  }
}

object PolynomialRingOverField {

  def apply[A, B <: Field[A]](p: FormalParameter, field: B): PolynomialRingOverField[A, B] = new PolynomialRingOverField[A, B] {

    override def param: FormalParameter = p

    override def coefficientField: B = field

  }
}
