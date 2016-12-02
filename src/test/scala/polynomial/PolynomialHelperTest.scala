package polynomial

import core.IntegerModding._
import org.scalatest.{FunSuite, Matchers}
import polynomial.AdjoiningOperations._
import polynomial.PolynomialHelper._
import polynomial.Predef.X

class PolynomialHelperTest extends FunSuite with Matchers {

  implicit val intsMod4 = IntegersMod(4)
  implicit val polyRing = intsMod4 r_adjoin X
  implicit def converter = intToResidueClass(4)

  //TODO: implement some kind of classOf method so you don't have to do this crappy addition
  val zero = intsMod4.zero
  val one = intsMod4.one
  val two = one + one
  val three = one + two

  test("trimLeadingZeros has no effect if the biggest coefficient has non-zero value") {

    val c = List(zero, one)

    trimLeadingZeros(c, intsMod4) should be (c)
  }

  test("trimLeadingZeros drops coefficients otherwise") {

    val c1 = List(zero, one, zero)

    trimLeadingZeros(c1, intsMod4) should be (List(zero, one))

    val c2 = List(zero, one, zero, zero)

    trimLeadingZeros(c2, intsMod4) should be (List(zero, one))
  }

  test("biggestKeyWithNonzeroValue returns Some(0) for constant (nonzero) functions") {

    val c1 = List(one)
    val c2 = List(one, zero)

    biggestKeyWithNonzeroValue(c1, intsMod4) should be (Some(0))
    biggestKeyWithNonzeroValue(c2, intsMod4) should be (Some(0))
  }

  test("biggestKeyWithNonzeroValue returns None for the zero polynomial") {

    val m0 = List()
    val m1 = List(zero)
    val m2 = List(zero, zero)

    biggestKeyWithNonzeroValue(m0, intsMod4) should be (None)
    biggestKeyWithNonzeroValue(m1, intsMod4) should be (None)
    biggestKeyWithNonzeroValue(m2, intsMod4) should be (None)
  }

  test("In general, biggestKeyWithNonzeroValue returns the degree") {

    val c1 = List(zero, one)
    val c2 = List(zero, zero, one)
    val c3 = List(zero, one, zero, one, zero)

    biggestKeyWithNonzeroValue(c1, intsMod4) should be (Some(1))
    biggestKeyWithNonzeroValue(c2, intsMod4) should be (Some(2))
    biggestKeyWithNonzeroValue(c3, intsMod4) should be (Some(3))
  }

  test("pairwiseBinaryListOp applies a binary operation on values that match to the same key") {

    val c1 = List(three, one)
    val c2 = List(two, zero)

    pairwiseBinaryListOp(intsMod4.plus, intsMod4.zero)(c1, c2) should be (List(one, one))
  }

  test("pairwiseBinaryListOp defaults to using the unit if one of the maps does not have a key in the other map") {

    val c1 = List(three, one, zero, three)
    val c2 = List(two, zero, one)

    pairwiseBinaryListOp(intsMod4.minus, intsMod4.zero)(c1, c2) should be (List(one, one, three, three))
  }

  test("mapCoefficientAndExtend is simply a map when extendBy is zero") {

    val c = List(two, one, zero, three)

    mapCoefficientsAndExtend(three, 0, c, intsMod4) should be (List(two, three, zero, one))
  }

  test("mapCoefficientsAndExtend just adds leading zeros (from the ring) after the map is applied") {

    val c = List(two, one, zero, three)

    mapCoefficientsAndExtend(three, 2, c, intsMod4) should be (List(zero, zero, two, three, zero, one))
  }

  test("multiplyByPowerOfParam does nothing if extendBy is zero") {

    val c = List(two, one, zero, three)
    multiplyByPowerOfParam(0, c, intsMod4) should be (c)
  }

  test("multiplyByPowerOfParam adds extendBy zeros to the front") {

    val c = List(two, one, zero, three)
    multiplyByPowerOfParam(2, c, intsMod4) should be (List(zero, zero, two, one, zero, three))
  }

}
