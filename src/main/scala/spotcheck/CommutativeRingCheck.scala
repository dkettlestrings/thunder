package spotcheck

import algebra.ring.CommutativeRing

object CommutativeRingCheck {

  def apply[A](elements: IndexedSeq[A])(implicit ring: CommutativeRing[A]): Boolean = {

    val pairs = elements zip elements
    val triples = elements map {x => (x, x, x)}

    def rightAdditiveIdentity(x: A) = ring.plus(x, ring.zero) == x

    def leftAdditiveIdentity(x: A) = ring.plus(ring.zero, x) == x

    def additiveCommutivity(tup: (A, A)): Boolean = tup match {
      case(x: A, y: A) =>
        ring.plus(x, y) == ring.plus(y, x)
    }

    def additiveAssociativity(tup: (A, A, A)): Boolean = tup match {
      case(x: A, y: A, z: A) =>
        ring.plus(x, ring.plus(y, z)) == ring.plus(ring.plus(x, y), z)
    }

    def rightMultiplicativeIdentity(x: A) = ring.times(x, ring.one) == x

    def leftMultiplicativeIdentity(x: A) = ring.times(ring.one, x) == x

    def multiplicativeCommutivity(tup: (A, A)): Boolean = tup match {
      case(x: A, y: A) =>
        ring.times(x, y) == ring.times(y, x)
    }

    def multiplicativeAssociativity(tup: (A, A, A)): Boolean = tup match {
      case(x: A, y: A, z: A) =>
        ring.times(x, ring.times(y, z)) == ring.times(ring.times(x, y), z)
    }

    def distribution(tup: (A, A, A)): Boolean = tup match {
      case(x: A, y: A, z: A) =>
        ring.times(x, ring.plus(y, z)) == ring.plus(ring.times(x, y), ring.times(x, z))
    }

    elements.forall(rightAdditiveIdentity) &&
    elements.forall(leftAdditiveIdentity) &&
    pairs.forall(additiveCommutivity) &&
    triples.forall(additiveAssociativity) &&
    elements.forall(rightMultiplicativeIdentity) &&
    elements.forall(leftMultiplicativeIdentity) &&
    pairs.forall(multiplicativeCommutivity) &&
    triples.forall(multiplicativeAssociativity) &&
    triples.forall(distribution)
  }

}
