package finitefields

import org.scalatest.{FunSuite, Matchers}

class PrimeFieldTest extends FunSuite with Matchers {

  test("The finite field of integers moudlo a prime can be constructed via a prime") {

    val intsMod5 = PrimeField(5)
    intsMod5 should be
  }

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

    val intsMod5 = PrimeField(5, checkPrimality = true)
    intsMod5 should be

    intercept[IllegalArgumentException](PrimeField(8, checkPrimality = true))
  }

  test("We still have all of the regular commutative ring stuff") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)

    intsMod5.plus(2,4) should be (intsMod5.residueClass(1))
    intsMod5.minus(2, 4) should be (intsMod5.residueClass(3))
    intsMod5.times(2, 4) should be (intsMod5.residueClass(3))
    intsMod5.pow(2, 4) should be (intsMod5.residueClass(1))
    intsMod5.negate(2) should be (intsMod5.residueClass(3))
    intsMod5.zero should be (intsMod5.residueClass(0))
    intsMod5.one should be (intsMod5.residueClass(1))
  }

  //TODO: Add inv of zero and divide by zero
  test("But now we have multiplicative inverses") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)

    intsMod5.inv(1) should be (intsMod5.residueClass(1))
    intsMod5.inv(2) should be (intsMod5.residueClass(3))
    intsMod5.inv(3) should be (intsMod5.residueClass(2))
    intsMod5.inv(4) should be (intsMod5.residueClass(4))
  }

  test("And of course, division") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)

    intsMod5.div(1, 1) should be (intsMod5.residueClass(1))
    intsMod5.div(4, 3) should be (intsMod5.residueClass(3))
    intsMod5.div(3, 4) should be (intsMod5.residueClass(2))
  }

  test("But these aren't defined for zero") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)

    intercept[ArithmeticException](intsMod5.inv(intsMod5.zero))

    intercept[ArithmeticException](intsMod5.div(3, intsMod5.zero))
  }

}
