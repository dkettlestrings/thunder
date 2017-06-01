package integer

import core.ModuloOperations._
import core.{FinitelyGenerable, Field, ResidueClass}

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

    private val delegate: Field[ResidueClass[Int]] = integers modulo_f p

    override def zero: ResidueClass[Int] = delegate.zero

    override def one: ResidueClass[Int] = delegate.one

    override def plus(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = delegate.plus(x, y)

    override def negate(x: ResidueClass[Int]): ResidueClass[Int] = delegate.negate(x)

    override def times(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = delegate.times(x, y)

    override def div(x: ResidueClass[Int], y: ResidueClass[Int]): ResidueClass[Int] = delegate.div(x, y)

    //TODO: update algebra dependency so I don't have to override this stuff see https://github.com/dkettlestrings/thunder/issues/15
    override def quot(a: ResidueClass[Int], b: ResidueClass[Int]): ResidueClass[Int] = delegate.quot(a, b)

    override def mod(a: ResidueClass[Int], b: ResidueClass[Int]): ResidueClass[Int] = delegate.mod(a, b)

  }
}
