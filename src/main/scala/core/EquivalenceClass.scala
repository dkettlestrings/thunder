package core

trait EquivalenceClass[A] {

  def relation: EquivalenceRelation[A]

  def representative: A

  def contains(a: A): Boolean = relation.areEquivalent(representative, a)

  //TODO: It would be nice to know if they use the same equivalence relation
  def ==(eq: EquivalenceClass[A]): Boolean = this.contains(eq.representative)

  def !=(eq: EquivalenceClass[A]): Boolean = !(this == eq)

  override def equals(obj: scala.Any): Boolean = obj match {
    case eq: EquivalenceClass[A] => this == eq
    case _ => false
  }

}

object EquivalenceClass {

  def apply[A](a: A, f: EquivalenceRelation[A]): EquivalenceClass[A] = {

    new EquivalenceClass[A] {
      override def representative = a

      override def relation = f
    }

  }
}
