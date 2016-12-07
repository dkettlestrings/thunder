package core

import algebra.ring.EuclideanRing

/**
  * Creates a EuclideanRing of Ints.
  */
object Integers {

  def apply(): EuclideanRing[Int] = new EuclideanRing[Int] {

    override def plus(x: Int, y: Int): Int = x + y

    override def one: Int = 1

    override def quot(a: Int, b: Int): Int = a / b

    override def negate(x: Int): Int = -x

    override def times(x: Int, y: Int): Int = x * y

    override def mod(a: Int, b: Int): Int = {
      val x = a % b
      if (x < 0) x + b else x
    }

    override def zero: Int = 0

  }



}
