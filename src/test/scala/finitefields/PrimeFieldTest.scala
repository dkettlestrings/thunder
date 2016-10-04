package finitefields

import org.scalatest.{FunSuite, Matchers}

class PrimeFieldTest extends FunSuite with Matchers {

  implicit def intsMod5 = PrimeField(5)
  implicit def converter = Converter(intsMod5)

  val zero = intsMod5.residueClass(0)
  val one = intsMod5.residueClass(1)
  val two = intsMod5.residueClass(2)
  val three = intsMod5.residueClass(3)
  val four = intsMod5.residueClass(4)

  test("In order to protect yourself, you can check for primality using isPrime") {

    isPrime(0) should be (false)
    isPrime(1) should be (false)
    isPrime(2) should be (true)
    isPrime(3) should be (true)
    isPrime(4) should be (false)
    isPrime(5) should be (true)
    isPrime(6) should be (false)
    isPrime(7) should be (true)
    isPrime(8) should be (false)
    isPrime(9) should be (false)
  }

  test("However, you can optionally check in the constructor to make sure the modulus is prime") {

    val intsMod7 = PrimeField(7, checkPrimality = true)
    intsMod7 should be

    intercept[IllegalArgumentException](PrimeField(8, checkPrimality = true))
  }

  test("Division works") {

    zero / one should be (zero)
    one / one should be (one)
    two / two should be (one)
    two / three should be (four)
    three / four should be (two)
    four / three should be (three)
  }

  test("Inversion works") {

    one.inv should be (one)
    two.inv should be (three)
    three.inv should be (two)
    four.inv should be (four)
  }

  test("You can't invert or divide by zero") {

    intercept[ArithmeticException](zero.inv)
    intercept[ArithmeticException](one / zero)
  }


  //TODO: Remove this test if non accepts the default quot and mod implementations
  test("A prime field has the Euclidean ring operations, but they're pointless") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)

    intsMod5.quot(1, 2) should be (one)
    intsMod5.quot(0, 2) should be (zero)
    intercept[ArithmeticException](intsMod5.quot(1, 0))

    intsMod5.mod(1, 2) should be (zero)
    intercept[ArithmeticException](intsMod5.mod(1, 0))
  }

}
