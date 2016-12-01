package core

import algebra.ring.{CommutativeRing, Field}
import core.CommonEuclideanModdingContexts.IntegerModdingContext
import core.ModuloOperations._

object IntegerModding {

  implicit def integers = Integers()

  def IntegersMod(n: Int): CommutativeRing[ResidueClass[Int]] = Integers() modulo_r n

  def PrimeField(p: Int): Field[ResidueClass[Int]] = Integers() modulo_f p

  def intToResidueClass(modulus: Int): Int => ResidueClass[Int] = x => ResidueClass(x, modulus)

}
