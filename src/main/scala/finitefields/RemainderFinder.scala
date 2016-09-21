package finitefields

import scala.annotation.tailrec

//TODO: Get rid of this and implement an Extended Euclidean Algorithm for Ints
object RemainderFinder {

  def apply(i: Int, m: Int): Int = {

    @tailrec
    def go(a: Int, b: Int): Int = {
      if(a >= 0) a
      else go(a + b, b)
    }

    val attempt = i % m
    if(attempt >= 0) attempt
    else go(attempt, m)

  }

}
