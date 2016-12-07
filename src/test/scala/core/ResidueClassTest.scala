package core

import org.scalatest.{FunSuite, Matchers}
import IntegerModding._
import ModuloOperations._

class ResidueClassTest extends FunSuite with Matchers {

  test("Residue class equality holds as expected") {

    implicit def intsMod4 = integers modulo_r 4
    def classOf = intToResidueClass(4)

    classOf(2) == classOf(2) should be (true)
    classOf(2) == classOf(6) should be (true)
    classOf(2) == classOf(-2) should be (true)
  }

  test("Residue class equality does not hold as expected") {

    implicit def intsMod4 = integers modulo_r 4
    def classOf = intToResidueClass(4)

    classOf(2) != classOf(3) should be (true)
    classOf(7) != classOf(0) should be (true)
  }

  test("Residue classes of different moduli cannot be equal") {

    def intsMod4 = integers modulo_r 4
    def mod4 = intToResidueClass(4)

    def intsMod6 = integers modulo_r 6
    def mod6 = intToResidueClass(6)

    mod4(2) != mod6(2) should be (true)
  }

  test("Residue class equality takes types into account") {

    implicit def intsMod4 = integers modulo_r 4
    def classOf = intToResidueClass(4)

    classOf(2) != 2
  }

}
