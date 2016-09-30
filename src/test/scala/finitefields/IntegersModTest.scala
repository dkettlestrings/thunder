package finitefields

import org.scalatest.{FunSuite, Matchers}
import spotcheck.CommutativeRingCheck

class IntegersModTest extends FunSuite with Matchers {

  implicit def intsMod4 = IntegersMod(4)
  implicit def converter = Converter(intsMod4)

  val zero = intsMod4.residueClass(0)
  val one = intsMod4.residueClass(1)
  val two = intsMod4.residueClass(2)
  val three = intsMod4.residueClass(3)

  test("Addition works") {

    zero + zero should be (zero)
    zero + one should be (one)
    one + zero should be (one)
    one + three should be (zero)
    two + two should be (zero)
    two + three should be (one)
    three + two should be (one)
  }

  test("Subtraction works") {

    zero - zero should be (zero)
    zero - one should be (three)
    one - zero should be (one)
    one - three should be (two)
    two - two should be (zero)
    two - three should be (three)
    three - two should be (one)
  }

  test("Multiplication works") {

    zero * zero should be (zero)
    zero * one should be (zero)
    one * zero should be (zero)
    one * three should be (three)
    two * two should be (zero)
    two * three should be (two)
    three * two should be (two)
  }

  test("Exponentiation works") {

    zero ^ 0 should be (one) // TODO: Is this the behavior we want?
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

  test("Negation works") {

    intsMod4.negate(zero) should be (zero)
    intsMod4.negate(one) should be (three)
    intsMod4.negate(two) should be (two)
    intsMod4.negate(three) should be (one)
  }

  test("The ring has a zero and a one") {

    intsMod4.zero should be (zero)
    intsMod4.one should be (one)
  }

  test("It is a commutative ring") {

    val elements = (0 to 3) map {intsMod4.residueClass(_)}
    CommutativeRingCheck(elements) should be (true)
  }

}
