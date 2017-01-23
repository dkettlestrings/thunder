package core

import integer.integers
import ModuloOperations._
import org.scalatest.{FunSuite, Matchers}

class ResidueClassTest extends FunSuite with Matchers {

  def intToResidueClass(modulus: Int): Int => ResidueClass[Int] = x => ResidueClass(x, modulus)

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

  //TODO: suppress compiler warnings for equality checking on incompatible types.  See https://github.com/dkettlestrings/thunder/issues/46
  test("Residue class equality takes types into account") {

    implicit def intsMod4 = integers modulo_r 4
    def classOf = intToResidueClass(4)

    classOf(2) != 2
  }

  test("Residue classes can be used in Sets") {

    implicit def intsMod4 = integers modulo_r 4
    def classOf = intToResidueClass(4)

    val set = Set(classOf(0), classOf(1), classOf(5))

    set.size should be (2)
    set.contains(classOf(8)) should be (true)
    set.contains(classOf(7)) should be (false)
  }

  test("Residue classes are printed in square bracket notation") {

    def intsMod4 = integers modulo_r 4
    def mod4 = intToResidueClass(4)
    mod4(3).toString should be ("[3]_4")
  }

}
