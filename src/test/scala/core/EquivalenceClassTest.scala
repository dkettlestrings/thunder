package core

import org.scalatest.{FunSuite, Matchers}

class EquivalenceClassTest extends FunSuite with Matchers {

  def relation = EquivalenceRelation((x: Int) => x % 2)

  val classForZero = EquivalenceClass(0, relation)
  val classForTwo = EquivalenceClass(2, relation)
  val classForFour = EquivalenceClass(4, relation)

  val classForOne = EquivalenceClass(1, relation)
  val classForThree = EquivalenceClass(3, relation)

  test("Equivalence classes are equal when their representatives evaluate to the same value under the equivalence relation") {

    classForZero == classForTwo should be (true)
    classForZero == classForFour should be (true)

    //You can also write it this way
    classForZero should be (classForTwo)
    classForTwo should be (classForFour)
  }

  test("Equivalence classes are not equal when their representatives do not evaluate to the same value under the equivalence relation") {

    classForZero != classForOne should be (true)
    classForZero != classForThree should be (true)

    //You can also write it this way
    classForZero should not be classForOne
    classForZero should not be classForThree
  }

  test("You can ask an equivalence class if it contains an element") {

    classForZero.contains(6) should be (true)
    classForZero.contains(7) should be (false)
  }

  test("Equality checks on equivalence classes do type checks") {

    classForZero.equals(0) should be (false)
  }
}
