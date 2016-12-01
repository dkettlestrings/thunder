package core

// Used for modding a Euclidean domain by an irreducible element to get a field.
trait EuclideanRingModdingContext[A] {

  def elementsUpTo(a: A): IndexedSeq[A]

}

object CommonEuclideanModdingContexts {


  implicit def IntegerModdingContext: EuclideanRingModdingContext[Int] = {

    new EuclideanRingModdingContext[Int] {

      override def elementsUpTo(a: Int): IndexedSeq[Int] = 1 to a
    }
  }
}
