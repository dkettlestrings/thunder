package polynomial

import org.scalatest.{FunSuite, Matchers}

class ExtendedIntegerTest extends FunSuite with Matchers {

  test("Extended integers have an ordering") {

    FiniteInteger(1) <  FiniteInteger(2) should be (true)
    FiniteInteger(1) <= FiniteInteger(2) should be (true)
    FiniteInteger(1) <= FiniteInteger(1) should be (true)

    FiniteInteger(1) < PositiveInfinity should be (true)
    NegativeInfinity < FiniteInteger(1) should be (true)
    NegativeInfinity < PositiveInfinity should be (true)

    FiniteInteger(1) > NegativeInfinity should be (true)
    PositiveInfinity > FiniteInteger(1) should be (true)
    PositiveInfinity > NegativeInfinity should be (true)

    // Note we explicitly call compare() here because == will not.
    PositiveInfinity.compare(PositiveInfinity) should be (0)
    NegativeInfinity.compare(NegativeInfinity) should be (0)
  }

  test("You can add and subtract finite integers as expected") {

    FiniteInteger(2) + FiniteInteger(3) should be (FiniteInteger(5))
    FiniteInteger(2) - FiniteInteger(3) should be (FiniteInteger(-1))
  }

  test("Adding and subtracting positive infinity from finite values works as expected") {

    PositiveInfinity + FiniteInteger(1) should be (PositiveInfinity)
    FiniteInteger(1) + PositiveInfinity should be (PositiveInfinity)

    PositiveInfinity - FiniteInteger(1) should be (PositiveInfinity)
    FiniteInteger(1) - PositiveInfinity should be (NegativeInfinity)
  }

  test("Adding and subtracting negative infinity from finite values works as expected") {

    NegativeInfinity + FiniteInteger(1) should be (NegativeInfinity)
    FiniteInteger(1) + NegativeInfinity should be (NegativeInfinity)

    NegativeInfinity - FiniteInteger(1) should be (NegativeInfinity)
    FiniteInteger(1) - NegativeInfinity should be (PositiveInfinity)
  }

  test("Adding and subtracting infinities does not always work") {

    PositiveInfinity + PositiveInfinity should be (PositiveInfinity)
    intercept[ArithmeticException](PositiveInfinity - PositiveInfinity)

    intercept[ArithmeticException](NegativeInfinity + PositiveInfinity)
    NegativeInfinity - PositiveInfinity should be (NegativeInfinity)

    intercept[ArithmeticException](PositiveInfinity + NegativeInfinity)
    PositiveInfinity - NegativeInfinity should be (PositiveInfinity)

    NegativeInfinity + NegativeInfinity should be (NegativeInfinity)
    intercept[ArithmeticException](NegativeInfinity - NegativeInfinity)
  }

  test("You can convert finite integers to regular Ints, but not the infinite ones") {

    FiniteInteger(3).toInt should be (3)
    intercept[ArithmeticException](PositiveInfinity.toInt)
    intercept[ArithmeticException](NegativeInfinity.toInt)
  }
}
