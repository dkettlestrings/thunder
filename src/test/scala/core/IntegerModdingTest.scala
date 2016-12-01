package core

import org.scalatest.{FunSuite, Matchers}
import IntegerModding._

class IntegerModdingTest extends FunSuite with Matchers {

  test("You can easily create the ring of integers modulo a number") {

    implicit val intsMod4 = IntegersMod(4)
    intsMod4.zero + intsMod4.one should be (intsMod4.one)
  }

  test("You can easily create the field of integers modulo a prime") {

    implicit val intsMod3 = PrimeField(3)
    intsMod3.zero + intsMod3.one should be (intsMod3.one)
  }



}
