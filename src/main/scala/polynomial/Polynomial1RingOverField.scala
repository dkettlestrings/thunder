package polynomial

import algebra.ring.{CommutativeRing, EuclideanRing, Field}

import scala.annotation.tailrec

trait Polynomial1RingOverField[A, B <: Field[A]] extends Polynomial1Ring[A] with EuclideanRing[Polynomial1[A]] {

  implicit def coefficientField: B

  implicit def polyRing = this

  override implicit def coefficientRing: CommutativeRing[A] = coefficientField

  override def quot(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = {

    @tailrec
    def go(a: Polynomial1[A], b: Polynomial1[A], acc: Polynomial1[A]): Polynomial1[A] = {
      if(a.degree < b.degree) acc
      else {
        val leadingCoefficient = coefficientField.div(a.leadingCoefficient, b.leadingCoefficient)
        val degreeDifference = a.degree - b.degree
        val xToTheN = polynomial(coefficientField.one, coefficientField.zero) ^ degreeDifference // X^(degreeDifference)
        val newQuot = xToTheN * polynomial(leadingCoefficient)
        val reducedA = a - (newQuot * b)
        go(reducedA, b, acc + newQuot)
      }
    }

    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else go(x, y, zero)

  }

  override def mod(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = {
    x - (quot(x, y) * y)
  }
}

object Polynomial1RingOverField {

  def apply[A, B <: Field[A]](p: FormalParameter, field: B): Polynomial1RingOverField[A, B] = new Polynomial1RingOverField[A, B] {

    override def param: FormalParameter = p

    override def coefficientField: B = field

  }
}
