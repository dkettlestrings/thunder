package finitefields

import org.scalatest.{FunSuite, Matchers}

class IntegersModTest extends FunSuite with Matchers {

  test("The integers modulo n are created by supplying n") {

    val intsMod4 = IntegersMod(4)
  }

  test("You can add elements in the integers modulo n in two ways") {

    val intsMod4 = IntegersMod(4)

    intsMod4.plus(ResidueClass(3, 4), ResidueClass(2, 4)) should be (ResidueClass(1, 4))

    // The fancy way if you define an implicit.
    implicit def converter = Converter(intsMod4)
    intsMod4.plus(3, 2) should be(ResidueClass(1, 4))
  }

  test("However, you are discouraged from created residue classes yourself") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)
    intsMod4.plus(3, 2) should be (intsMod4.residueClass(1))
  }

  test("You can similarly do subtraction and multiplication") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)

    intsMod4.minus(2, 3) should be (intsMod4.residueClass(3))
    intsMod4.times(3, 3) should be (intsMod4.residueClass(1))
  }

  test("Exponentiation is also a thing") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)

    intsMod4.pow(3, 2) should be (intsMod4.residueClass(1))
    intsMod4.pow(2, 3) should be (intsMod4.residueClass(0))

  }

  test("It also has negation") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)

    intsMod4.negate(1) should be (intsMod4.residueClass(3))

  }

  test("They also have handy identifiers for common information") {

    val intsMod4 = IntegersMod(4)

    intsMod4.modulus should be (4)
    intsMod4.one should be (intsMod4.residueClass(1))
    intsMod4.zero should be (intsMod4.residueClass(0))
  }

  test("The two identity elements work as expected") {

    val intsMod4 = IntegersMod(4)
    implicit def converter = Converter(intsMod4)

    intsMod4.plus(3, intsMod4.zero) should be (intsMod4.residueClass(3))
    intsMod4.plus(intsMod4.zero, 3) should be (intsMod4.residueClass(3))

    intsMod4.times(3, intsMod4.one) should be (intsMod4.residueClass(3))
    intsMod4.times(intsMod4.one, 3) should be (intsMod4.residueClass(3))

    intsMod4.times(3, intsMod4.zero) should be (intsMod4.zero)
    intsMod4.times(intsMod4.zero, 3) should be (intsMod4.zero)
  }

}
