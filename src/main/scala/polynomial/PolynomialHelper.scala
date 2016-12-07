package polynomial

import algebra.ring.CommutativeRing

import scala.annotation.tailrec

//TODO: Investigate whether this logic can be simplified or moved
/**
  * Grab-bag object for assorted operations on [[Polynomial]]s.
  *
  * The unifying theme of the functions in this object is that they all do low-level, implementation-specific operations
  * on [[Polynomial]]s.
  */
private[polynomial] object PolynomialHelper {

  @tailrec
  def trimLeadingZeros[A](a: List[A], ring: CommutativeRing[A]): List[A] = {
    if (a.isEmpty) a
    else if (a.last == ring.zero) trimLeadingZeros(a.init, ring)
    else a
  }

  def biggestKeyWithNonzeroValue[A](a: List[A], ring: CommutativeRing[A]): Option[Int] = {
    trimLeadingZeros(a, ring).size match {
      case 0 => None
      case n => Some(n - 1)
    }
  }

  def pairwiseBinaryListOp[A](op: (A, A) => A, unit: A)(x: List[A], y: List[A]): List[A] = {

    @tailrec
    def padWithUnitsUntil(a: List[A], length: Int): List[A] = {
      if (a.size >= length) a
      else padWithUnitsUntil(a :+ unit, length)
    }

    val maxSize = List(x.size, y.size).max
    val normalizedX = padWithUnitsUntil(x, maxSize)
    val normalizedY = padWithUnitsUntil(y, maxSize)

    normalizedX zip normalizedY map { case (a, b) => op(a, b) }
  }

  def mapCoefficientsAndExtend[A]( value: A, extendBy: Int, x: List[A], ring: CommutativeRing[A]): List[A] = {
    List.fill(extendBy)(ring.zero) ++ x.map(c => ring.times(value, c))
  }

  def multiplyByPowerOfParam[A](extendBy: Int, x: List[A], ring: CommutativeRing[A]): List[A] = {
    mapCoefficientsAndExtend(ring.one, extendBy, x, ring)
  }

  def polynomialFromList[A](coefficients: List[A])(implicit param: FormalParameter, coefficientRing: CommutativeRing[A]): Polynomial[A] = Polynomial[A](param, trimLeadingZeros[A](coefficients, coefficientRing))

}
