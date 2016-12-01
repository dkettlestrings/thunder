package core

import algebra.ring.EuclideanRing

// Used for modding a Euclidean domain by an irreducible element to get a field.
trait EuclideanRingModdingContext[A] {

  def domain: EuclideanRing[A]

  def elemmentsUpTo(a: A): IndexedSeq[A]

}

object CommonEuclideanModdingContexts {


  implicit def IntegerModdingContext: EuclideanRingModdingContext[Int] = {

    new EuclideanRingModdingContext[Int] {

      override def domain: EuclideanRing[Int] = Integers()

      override def elemmentsUpTo(a: Int): IndexedSeq[Int] = 1 to a
    }
  }
}
