package rationalfunction

import finitefields.{Converter, PrimeField, ResidueClass}
import org.scalatest.{FunSuite, Matchers}
import polynomial.Polynomial1RingOverField
import polynomial.Predef.{X, Y}
import rationalfunction.FFGenerator.extend

class FFGeneratorTest extends FunSuite with Matchers {

  test("extend the rational functions") {

    implicit def intsMod5 = PrimeField(5)
    implicit def converter = Converter(intsMod5)
    implicit def rationalFunctions = RationalFunctionField[ResidueClass, PrimeField](X, intsMod5)
    implicit def originalPolyRing = rationalFunctions.polynomialRing
    val justX = originalPolyRing.polynomial(1, 0)
    val xAsRationalFunction = rationalFunctions.rationalFunction(justX, originalPolyRing.one)

    implicit def polynomialsOverXAndY = Polynomial1RingOverField[RationalFunction[ResidueClass, PrimeField], FunctionField[ResidueClass, PrimeField]](Y, rationalFunctions)
    val relation = polynomialsOverXAndY.polynomial(xAsRationalFunction, rationalFunctions.one) // YX + 1 = 0

    implicit def extensionField = extend(rationalFunctions, relation)


    val p = extensionField.polynomialRing.polynomial(rationalFunctions.one, rationalFunctions.zero)
    val q = extensionField.polynomialRing.one

    val y = extensionField.rationalFunction(p, q)


    println(y)
  }

}
