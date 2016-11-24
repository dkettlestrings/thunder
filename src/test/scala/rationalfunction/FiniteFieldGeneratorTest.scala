package rationalfunction

import finitefields.{Converter, PrimeField}
import org.scalatest.{FunSuite, Matchers}

class FiniteFieldGeneratorTest extends FunSuite with Matchers {

  test("extends a field") {

    val intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)

    implicit def extensionField = FiniteFieldGenerator.extend(intsMod5, 3)

    val p = extensionField.polynomialRing.polynomial(1, 0)
    val q = extensionField.polynomialRing.one

    val x = extensionField.rationalFunction(p, q)

    (x ^ 3) - x === extensionField.zero should be (true)
  }

}
