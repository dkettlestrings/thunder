package polynomial

import algebra.ring.CommutativeRing

import scala.annotation.tailrec

//TODO: Switch to infix notation and use implicit rings
private[polynomial] object PolynomialHelper {

  @tailrec
  def trimLeadingZeros[A](a: List[A], ring: CommutativeRing[A]): List[A] = {
    if (a.size == 0) return a
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
}
