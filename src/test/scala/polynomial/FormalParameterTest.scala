package polynomial

import org.scalatest.{FunSuite, Matchers}

class FormalParameterTest extends FunSuite with Matchers {

  test("You can create a formal parameter with any string you want, but single characters are conventional") {

    val questionMark = FormalParameter("?")
    questionMark.name should be ("?")
    questionMark.toString should be ("?")
  }

}
