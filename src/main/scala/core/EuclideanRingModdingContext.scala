package core

/**
  * Encapsulates the information required to find inverses when modding a EuclideanRing by an irreducible element.
  *
  * When you mod an irreducible element from a EuclideanRing, you generate a Field.
  * In order to find inverses or do division within this field, you need to know all of the possible elements that might
  * serve as an inverse.  In practice, this means finding all of the elements "smaller" than the irreducible element
  * (and not zero).  For example, when working within the Integers modulo a prime p, the possible inverses for any
  * element are the numbers 1 to p-1.  When working with a polynomial ring modulo an irreducible polynomial p, the
  * possible inverses are all non-zero polynomials of degree <= degree(p).
  *
  * @tparam A The type of the elements within the EuclideanRing (their type parameters should match).  For example, you
  *           must use EuclideanRingModdingContext[Foo] when working with a EuclideanRing[Foo]
  */
trait EuclideanRingModdingContext[A] {

  def elementsUpTo(a: A): IndexedSeq[A]

}
