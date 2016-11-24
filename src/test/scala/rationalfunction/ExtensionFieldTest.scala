package rationalfunction

import finitefields.{Converter, PrimeField, ResidueClass}
import polynomial.Predef.{X, Y}
import org.scalatest.{FunSuite, Matchers}
import polynomial.Polynomial1RingOverField

class ExtensionFieldTest extends FunSuite with Matchers {

  test("Extend a Prime field") {

    val baseField = PrimeField(5)
    implicit def converter = Converter(baseField)
    implicit def polyRing = Polynomial1RingOverField[ResidueClass, PrimeField](X, baseField)
    val p = polyRing.polynomial(1, 0, 2)
    implicit def extensionField = Extend(X, baseField, p)

    println(extensionField.adjoined)


  }

}
