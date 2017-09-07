package integer

import core.ModuloOperations._
import core.{Finite, Field, ResidueClass}
import core.InfixOps._

/**
  * Describes the Field of the integers modulo a prime.
  *
  */
trait IntegersModP extends Field[ResidueClass[Int]] with IntegersModN with Finite[ResidueClass[Int]]

/**
  * Companion object for creating instances.
  */
object IntegersModP {

  /**
    * Creates an instance of the Field of integers modulo a prime.
    * @param p The prime modulus
    * @return
    */
  def apply(p: Int): IntegersModP = new IntegersModP {

    override lazy val modulus: Int = p

    private implicit val delegate: Field[ResidueClass[Int]] = integers modulo_f p

    override def div(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = x / y

  }
}
