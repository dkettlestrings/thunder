package core

/**
  * Encapsulates the information required to find inverses when modding a EuclideanRing by an irreducible element.
  *
  * When you mod an irreducible element from a EuclideanRing, you generate a field.  In order to find inverses or do
  * division within this field, you need to know all of the possible elements that might serve as an inverse.  In
  * practice, this means finding all of the elements "smaller" than the irreducible element (and not zero).  For example,
  * when working within the Integers modulo a prime p, the possible inverses for any element are the numbers 1 to p-1.
  * When working with a polynomial ring modulo an irreducible polynomial p, the possible inverses are all polynomials of
  * degree <= degree(p).
  *
  * @tparam A
  */
trait EuclideanRingModdingContext[A] {

  def elementsUpTo(a: A): IndexedSeq[A]

}

object CommonEuclideanModdingContexts {


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
