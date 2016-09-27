package finitefields

import algebra.ring.CommutativeRing


trait IntegersMod extends CommutativeRing[ResidueClass] {

  def modulus: Int

  def residueClass(n: Int): ResidueClass = ResidueClass(RemainderFinder(n, modulus), modulus)

  override def plus(x: ResidueClass, y: ResidueClass): ResidueClass = {
    require(x.modulus == modulus && y.modulus == modulus)
    residueClass(x.residue + y.residue)
  }

  override def one: ResidueClass = residueClass(1)

  override def negate(x: ResidueClass): ResidueClass = residueClass(-x.residue)

  override def times(x: ResidueClass, y: ResidueClass): ResidueClass = {
    require(x.modulus == modulus && y.modulus == modulus)
    residueClass(x.residue * y.residue)
  }

  override def zero: ResidueClass = residueClass(0)

}

object IntegersMod {

  def apply(n: Int): IntegersMod = new IntegersMod {
    override def modulus = n

  }
}

object Converter {

  def apply(im: IntegersMod): (Int) => ResidueClass = (i: Int) => im.residueClass(i)
}

case class ResidueClass(residue: Int, modulus: Int)






