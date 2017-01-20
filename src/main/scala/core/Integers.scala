package core

import algebra.ring.EuclideanRing

/**
  * Creates a EuclideanRing of Ints.
  *
  * It is recommended that you use [[IntegerModding.integers]] as opposed to instantiating your own.
  */
object Integers {

  def apply(): EuclideanRing[Int] = new EuclideanRing[Int] {

    override def zero: Int = 0

    override def one: Int = 1

    override def plus(x: Int, y: Int): Int = x + y

    override def negate(x: Int): Int = -x

    override def times(x: Int, y: Int): Int = x * y

    override def mod(a: Int, b: Int): Int = {
      val x = a % b
      if (x < 0) x + b else x
    }

    override def quot(a: Int, b: Int): Int = a / b

  }
}
