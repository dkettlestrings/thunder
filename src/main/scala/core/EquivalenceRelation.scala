package core

trait EquivalenceRelation[A] {

  //TODO: can we implement an equals on equivalence relations?  For a functional implementation, I don't see how.
  def areEquivalent(x: A, y: A): Boolean

}

object EquivalenceRelation {

  def apply[A, B](f: A => B): EquivalenceRelation[A] = {

    new EquivalenceRelation[A] {

      override def areEquivalent(x: A, y: A): Boolean = f(x) == f(y)
    }
  }
}
