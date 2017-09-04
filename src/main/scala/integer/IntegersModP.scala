package integer

import core.ModuloOperations._
import core.{FinitelyGenerable, Field, ResidueClass}
import core.FieldOps._

/**
  * Describes the Field of the integers modulo a prime.
  *
  */
trait IntegersModP extends Field[ResidueClass[Int]] with IntegersModN with FinitelyGenerable[ResidueClass[Int]]

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

    override val modulus: Int = p

    private implicit val delegate: Field[ResidueClass[Int]] = integers modulo_f p

    override def zero: ResidueClass[Int] = delegate.zero

    override def one: ResidueClass[Int] = delegate.one

    override def plus(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = x + y

    override def negate(x: ResidueClass[Int]): ResidueClass[Int] = x.negate

    override def times(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = x * y

    override def div(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = x / y

  }
}
