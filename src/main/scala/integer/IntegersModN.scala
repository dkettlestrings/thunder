package integer

import algebra.ring.CommutativeRing
import core.ModuloOperations._
import core.{Finite, ResidueClass}
import core.InfixOps._

/**
  * Describes the CommutativeRing of the integers modulo a number.
  *
  * See https://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n
  */
trait IntegersModN extends CommutativeRing[ResidueClass[Int]] with Finite[ResidueClass[Int]] {


  val modulus: Int

  def classOf(r: Int): ResidueClass[Int] = ResidueClass(r, modulus)

  override def elements: Set[ResidueClass[Int]] = (0 until modulus).map(classOf).toSet

  private implicit val delegate: CommutativeRing[ResidueClass[Int]] = integers modulo_r modulus

  override def zero: ResidueClass[Int] = delegate.zero

  override def one: ResidueClass[Int] = delegate.one

  override def plus(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = x + y

  override def negate(x: ResidueClass[Int]): ResidueClass[Int] = x.negate

  override def times(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = x * y

}

/**
  * Companion object for creating instances.
  */
object IntegersModN {

  /**
    * Creates a CommutativeRing of the integers modulo a number.
    * @param n The modulus
    * @return
    */
  def apply(n: Int): IntegersModN = new IntegersModN {

    override lazy val modulus: Int = n
  }
}
