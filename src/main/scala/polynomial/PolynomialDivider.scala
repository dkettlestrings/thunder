package polynomial

import algebra.ring.Field

import scala.annotation.tailrec

//TODO: use Euclidean algorithm for polynomials
object PolynomialDivider {

  //TODO: add multiplying by a power of a polynomial to Polynomial1Ring  also use that in PolynomialHelper
  //TODO Get rid of this stupid B type parameter
  def quot[A, B <: Field[A]](x: Polynomial1[A], y: Polynomial1[A])(implicit polyRing: Polynomial1Ring[A], field: B): Polynomial1[A] = {
    @tailrec
    def go(a: Polynomial1[A], b: Polynomial1[A], acc: Polynomial1[A]): Polynomial1[A] = {
      if(polyRing.degree(a) < polyRing.degree(b)) acc
      else {
        val leadingCoefficient = field.div(polyRing.leadingCoefficient(a), polyRing.leadingCoefficient(b))
        val degreeDifference = polyRing.degree(a) - polyRing.degree(b)
        val xToTheN = polyRing.pow(polyRing.polynomial(polyRing.ring.one, polyRing.ring.zero), degreeDifference) // X^(degreeDifference)
        val newQuot = polyRing.times(xToTheN, polyRing.polynomial(leadingCoefficient))
        val reducedA = polyRing.minus(a, polyRing.times(newQuot, b))
        go(reducedA, b, polyRing.plus(acc, newQuot))
      }
    }

    if(y == polyRing.zero) throw new ArithmeticException("Dividing by zero")
    else go(x, y, polyRing.zero)

  }

  def mod[A, B <: Field[A]](x: Polynomial1[A], y: Polynomial1[A])(implicit polyRing: Polynomial1Ring[A], field: B): Polynomial1[A] = {
    polyRing.minus(x, polyRing.times(quot(x, y), y))
  }

}
