package core

/**
  * A mechanism for determining whether two elements of A are equivalent.
  *
  * An EquivalenceRelation must satisfy the following three properties for all a, b, and c in A:
  *
  * 1. areEquivalent(a, a) (reflexive)
  *
  * 2. If areEquivalent(a, b) then areEquivalent(b, a) (symmetric)
  *
  * 3. If areEquivalent(a, b) and areEquivalent(b, c) then areEquivalent(a, c) (transitive)
  * @tparam A
  */
trait EquivalenceRelation[A] {

  //TODO: can we implement an equals on equivalence relations?  For a functional implementation, I don't see how.
  def areEquivalent(x: A, y: A): Boolean

}

object EquivalenceRelation {

  /**
    * Creates an EquivalenceRelation using a function.
    *
    * Once constructed, the EquivalenceRelation will equate elements a, b in A if and only if f(a) == f(b).
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def apply[A, B](f: A => B): EquivalenceRelation[A] = {

    new EquivalenceRelation[A] {

      override def areEquivalent(x: A, y: A): Boolean = f(x) == f(y)
    }
  }
}
