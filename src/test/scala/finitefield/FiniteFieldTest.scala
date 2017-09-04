package finitefield

import core.InfixOps._
import org.scalatest.{FunSuite, Matchers}

class FiniteFieldTest extends FunSuite with Matchers {

  test("A finite field with 3 elements should be Z/3Z") {

    implicit val ff = FiniteField(3, 1)
    ff.elements.size should be (3)
    ff.characteristic should be (3)

    val zero = ff.zero
    val one = ff.one
    val two = one + one

    ff.elements should be (Set(zero, one, two))
    ff.elements.foreach(x =>
      x + x + x should be (zero)
    )

    zero + zero should be (zero)
    one + one should be (two)
    one + two should be (zero)

    two * zero should be (zero)
    two * two should be (one)
  }

  //TODO: get classOf on the finite field and make tests that validate arithmetic properties of elements.  See https://github.com/dkettlestrings/thunder/issues/63
  test("A finite field of 9 elements should be constructable") {

    implicit val ff = FiniteField(3, 2)
    ff.elements.size should be (9)
    ff.characteristic should be (3)

    ff.elements.foreach(x =>
      x + x + x should be (ff.zero)
    )

  }
}
