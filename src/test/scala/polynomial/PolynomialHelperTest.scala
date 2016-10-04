package polynomial

import org.scalatest.{FunSuite, Matchers}
import PolynomialHelper.{biggestKeyWithNonzeroValue, mapCoefficientsAndExtend, multiplyByPowerOfParam, pairwiseBinaryListOp, trimLeadingZeros}
import finitefields.{Converter, IntegersMod}

class PolynomialHelperTest extends FunSuite with Matchers {

  val ring = IntegersMod(4)
  implicit def converter = Converter(ring)
  val zero = ring.residueClass(0)
  val one = ring.residueClass(1)
  val two = ring.residueClass(2)
  val three = ring.residueClass(3)

  test("trimLeadingZeros has no effect if the biggest coefficient has non-zero value") {

    val c = List(zero, one)

    trimLeadingZeros(c, ring) should be (c)
  }

  test("trimLeadingZeros drops coefficients otherwise") {

    val c1 = List(zero, one, zero)

    trimLeadingZeros(c1, ring) should be (List(zero, one))

    val c2 = List(zero, one, zero, zero)

    trimLeadingZeros(c2, ring) should be (List(zero, one))
  }

  test("biggestKeyWithNonzeroValue returns Some(0) for constant (nonzero) functions") {

    val c1 = List(one)
    val c2 = List(one, zero)

    biggestKeyWithNonzeroValue(c1, ring) should be (Some(0))
    biggestKeyWithNonzeroValue(c2, ring) should be (Some(0))
  }

  test("biggestKeyWithNonzeroValue returns None for the zero polynomial") {

    val m0 = List()
    val m1 = List(zero)
    val m2 = List(zero, zero)

    biggestKeyWithNonzeroValue(m0, ring) should be (None)
    biggestKeyWithNonzeroValue(m1, ring) should be (None)
    biggestKeyWithNonzeroValue(m2, ring) should be (None)
  }

  test("In general, biggestKeyWithNonzeroValue returns the degree") {

    val c1 = List(zero, one)
    val c2 = List(zero, zero, one)
    val c3 = List(zero, one, zero, one, zero)

    biggestKeyWithNonzeroValue(c1, ring) should be (Some(1))
    biggestKeyWithNonzeroValue(c2, ring) should be (Some(2))
    biggestKeyWithNonzeroValue(c3, ring) should be (Some(3))
  }

  test("pairwiseBinaryListOp applies a binary operation on values that match to the same key") {

    val c1 = List(three, one)
    val c2 = List(two, zero)

    pairwiseBinaryListOp(ring.plus, ring.zero)(c1, c2) should be (List(one, one))
  }

  test("pairwiseBinaryListOp defaults to using the unit if one of the maps does not have a key in the other map") {

    val c1 = List(three, one, zero, three)
    val c2 = List(two, zero, one)

    pairwiseBinaryListOp(ring.minus, ring.zero)(c1, c2) should be (List(one, one, three, three))
  }

  test("mapCoefficientAndExtend is simply a map when extendBy is zero") {

    val c = List(two, one, zero, three)

    mapCoefficientsAndExtend(three, 0, c, ring) should be (List(two, three, zero, one))
  }

  test("mapCoefficientsAndExtend just adds leading zeros (from the ring) after the map is applied") {

    val c = List(two, one, zero, three)

    mapCoefficientsAndExtend(three, 2, c, ring) should be (List(zero, zero, two, three, zero, one))
  }

  test("multiplyByPowerOfParam does nothing if extendBy is zero") {

    val c = List(two, one, zero, three)
    multiplyByPowerOfParam(0, c, ring) should be (c)
  }

  test("multiplyByPowerOfParam adds extendBy zeros to the front") {

    val c = List(two, one, zero, three)
    multiplyByPowerOfParam(2, c, ring) should be (List(zero, zero, two, one, zero, three))
  }

}
