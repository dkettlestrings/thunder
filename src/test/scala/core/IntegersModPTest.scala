package core

import org.scalatest.{FunSuite, Matchers}
import ModuloOperations.toModdable

class IntegersModPTest extends FunSuite with Matchers {

  implicit def context = CommonEuclideanModdingContexts.IntegerModdingContext
  implicit def intsMod3 = Integers() modulo_f 3

  val zero = intsMod3.zero
  val one = intsMod3.one
  val two = one + one

  test("Integers modulo a value should be an abelian group under addition") {

    zero + zero should be (zero)
    zero + one should be (one)
    one + zero should be (one)
    one + one should be (two)
    one + two should be (zero)
    two + one should be (zero)
    two + two should be (one)
  }

  test("Integers modulo a value should the appropriate multiplication structure") {

    zero * zero should be (zero)
    zero * one should be (zero)
    one * two should be (two)
    two * two should be (one)
  }

  test("Division works") {

    intercept[IllegalArgumentException](one / zero)

    zero / one should be (zero)
    one / one should be (one)
    one / two should be (two)
    two / one should be (two)
    two / two should be (one)
  }

  test("Inversion works") {

    intercept[IllegalArgumentException](zero.inv)
    one.inv should be (one)
    two.inv should be (two)
  }

  test("Doing a mod to create a field with a non-irreducible element gives runtime errors") {

    val intsMod4 = Integers() modulo_f 4
    val one_4 = intsMod4.one
    val two_4 = intsMod4.plus(one, one)

    intercept[RuntimeException](intsMod4.div(one_4, two_4))
  }

}
