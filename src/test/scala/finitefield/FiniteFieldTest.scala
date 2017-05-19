package finitefield

import org.scalatest.{FunSuite, Matchers}

class FiniteFieldTest extends FunSuite with Matchers {

  test("sandbox") {

    val ff = FiniteField(3, 1)
    ff.elements.size should be(3)


  }
}
