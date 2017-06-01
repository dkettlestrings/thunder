import core.EuclideanDomainModdingContext

/**
  * This package contains the EuclideanDomain of integers as well as types for the integers modulo a number.
  *
  * Note that you are encouraged to use the package-level [[integer.integers]] value as opposed to directly instantiating
  * [[integer.Integers]].
  */
package object integer {

  implicit val integers = Integers()

  /**
    * The EuclideanDomainModdingContext when creating the integers modulo a prime p.
    * @return
    */
  implicit def IntegerModdingContext: EuclideanDomainModdingContext[Int] = {

    new EuclideanDomainModdingContext[Int] {

      override def elementsUpTo(a: Int): IndexedSeq[Int] = 1 to a
    }
  }
}
