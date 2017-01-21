import core.EuclideanRingModdingContext

/**
  * This package contains the EuclideanRing of integers as well as types for the integers modulo a number.
  *
  * Note that you are encouraged to use the package-level [[integer.integers]] value as opposed to directly instantiating
  * [[integer.Integers]].
  */
package object integer {

  implicit val integers = Integers()

  /**
    * The EuclideanRingModdingContext when creating the integers modulo a prime p.
    * @return
    */
  implicit def IntegerModdingContext: EuclideanRingModdingContext[Int] = {

    new EuclideanRingModdingContext[Int] {

      override def elementsUpTo(a: Int): IndexedSeq[Int] = 1 to a
    }
  }
}
