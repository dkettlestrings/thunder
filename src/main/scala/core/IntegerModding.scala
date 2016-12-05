package core

import algebra.ring.{CommutativeRing, Field}
import core.CommonEuclideanModdingContexts.IntegerModdingContext
import core.ModuloOperations._

/**
  * Convenience methods for the integers modulo n.
  */
object IntegerModding {

  /**
    * The EuclideanRing of integers.
    * @return
    */
  implicit def integers = Integers()

  /**
    * Creates the CommutativeRing of integers modulo n.
    * @param n
    * @return
    */
  def IntegersMod(n: Int): CommutativeRing[ResidueClass[Int]] = Integers() modulo_r n

  /**
    * Creates the Field of integers modulo p.
    * @param p
    * @return
    */
  def PrimeField(p: Int): Field[ResidueClass[Int]] = Integers() modulo_f p

  /**
    * Allows for the conversion of integers to ResidueClasses.
    *
    * The primary purpose of this function is to allow for conversions of Ints to ResidueClasses.
    * Example:
    *
    * val intsMod4 = IntegersMod(4)
    * def classOf = intToResidueClass(4)
    * val one = classOf(1)
    * one + one == classOf(2)
    *
    * @param modulus
    * @return
    */
  def intToResidueClass(modulus: Int): Int => ResidueClass[Int] = x => ResidueClass(x, modulus)

}
