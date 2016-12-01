package core

import org.scalatest.{FunSuite, Matchers}

class EquivalenceRelationTest extends FunSuite with Matchers {

  test("An equivalence relation tells you if two elements are equivalent") {

    val relation = EquivalenceRelation[List[String], Int](_.length)

    relation.areEquivalent(List("a", "b", "c"), List("d", "e", "f")) should be (true)

    relation.areEquivalent(List("a", "b", "c"), List("d", "e")) should be (false)
  }

}
