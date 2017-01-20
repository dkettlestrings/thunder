package core

import org.scalatest.{FunSuite, Matchers}

class EquivalenceClassTest extends FunSuite with Matchers {

  def relation = EquivalenceRelation((x: Int) => x % 2)

  val classForZero = EquivalenceClass(0, relation)
  val classForTwo = EquivalenceClass(2, relation)
  val classForFour = EquivalenceClass(4, relation)

  val classForOne = EquivalenceClass(1, relation)
  val classForThree = EquivalenceClass(3, relation)

  test("You can ask an equivalence class if it contains an element") {

    classForZero.contains(6) should be (true)
    classForZero.contains(7) should be (false)
  }

  //TODO: suppress compiler warning about equality on different types.  Wait, if the compiler knows, then why is the signature Any/  See https://github.com/dkettlestrings/thunder/issues/46
  test("Equality checks on equivalence classes do type checks") {

    classForZero == 0 should be (false)
  }
}
