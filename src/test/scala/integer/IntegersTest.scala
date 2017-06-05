package integer

import org.scalatest.{FunSuite, Matchers}

class IntegersTest extends FunSuite with Matchers {

  val integers = Integers()

  test("The integers have the usual operations of a EuclideanDomain") {

    integers.plus(2, 3) should be (5)
    integers.one should be (1)
    integers.negate(1) should be (-1)
    integers.zero should be (0)
    integers.times(2, 3) should be (6)
    integers.quot(15, 7) should be (2)
    integers.mod(15, 7) should be (1)
  }

  test("The mod function behaves mathematically, not computer-sciency") {

    -3 % 4 should be (-3)
    integers.mod(-3, 4) should be (1)
  }

}
