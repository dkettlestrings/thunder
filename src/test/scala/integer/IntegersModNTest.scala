package integer

import core.CommutativeRingOps._
import org.scalatest.{FunSuite, Matchers}

class IntegersModNTest extends FunSuite with Matchers {

  implicit val intsMod4 = IntegersModN(4)

  val zero = intsMod4.classOf(0)
  val one = intsMod4.classOf(1)
  val two = intsMod4.classOf(2)
  val three = intsMod4.classOf(3)

  test("Integers modulo a value should have that many elements") {

    intsMod4.elements should be (Set(zero, one, two, three))
  }

  test("Integers modulo a value should be an abelian group under addition") {

    zero + zero should be (zero)
    zero + one should be (one)
    zero + two should be (two)
    zero + three should be (three)

    one + zero should be (one)
    one + one should be (two)
    one + two should be (three)
    one + three should be (zero)

    two + zero should be (two)
    two + one should be (three)
    two + two should be (zero)
    two + three should be (one)

    three + zero should be (three)
    three + one should be (zero)
    three + two should be (one)
    three + three should be (two)
  }

  test("Integers modulo a value should the appropriate multiplication structure") {

    zero * zero should be (zero)
    zero * one should be (zero)
    zero * two should be (zero)
    zero * three should be (zero)

    one * zero should be (zero)
    one * one should be (one)
    one * two should be (two)
    one * three should be (three)

    two * zero should be (zero)
    two * one should be (two)
    two * two should be (zero)
    two * three should be (two)

    three * zero should be (zero)
    three * one should be (three)
    three * two should be (two)
    three * three should be (one)
  }

  test("Negation works as expected, but you cant use it with a preceding negative sign") {

    assertDoesNotCompile("val a = -zero")
    zero.negate should be (zero)
    one.negate should be (three)
    two.negate should be (two)
    three.negate should be (one)
  }

  test("Subtraction works") {

    zero - zero should be (zero)
    zero - one should be (three)
    zero - two should be (two)
    zero - three should be (one)

    one - zero should be (one)
    one - one should be (zero)
    one - two should be (three)
    one - three should be (two)

    two - zero should be (two)
    two - one should be (one)
    two - two should be (zero)
    two - three should be (three)

    three - zero should be (three)
    three - one should be (two)
    three - two should be (one)
    three - three should be (zero)
  }

  test("Exponentiation works") {

    zero ^ 0 should be (one)
    zero ^ 1 should be (zero)
    zero ^ 2 should be (zero)
    zero ^ 3 should be (zero)

    one ^ 0 should be (one)
    one ^ 1 should be (one)
    one ^ 2 should be (one)
    one ^ 3 should be (one)

    two ^ 0 should be (one)
    two ^ 1 should be (two)
    two ^ 2 should be (zero)
    two ^ 3 should be (zero)

    three ^ 0 should be (one)
    three ^ 1 should be (three)
    three ^ 2 should be (one)
    three ^ 3 should be (three)
  }

}
