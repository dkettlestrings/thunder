package core

/**
  * The set of all elements of type A that are equivalent under some EquivalenceRelation.
  *
  * Given a particular Equivalence relation, the set of EquivalenceClasses form a partition for the elements of A.
  * @tparam A
  */
trait EquivalenceClass[A] {

  def relation: EquivalenceRelation[A]

  /**
    * A single element of the EquivalenceClass.
    * @return
    */
  def representative: A

  /**
    * A predicate to determine whether this EquivalenceClass contains the element.
    * @param a
    * @return
    */
  def contains(a: A): Boolean = relation.areEquivalent(representative, a)

  //TODO: It would be nice to know if they use the same equivalence relation
  /**
    * Determines whether the EquivalenceClasses are equal.
    *
    * This is done by checking whether each EquivalenceClass contains the representative element of the other.  Note
    * that this check is somewhat unsafe since it does not check whether the two EquivalenceRelations are the same.
    * This shortcoming is due to the fact that it would require equality checks for functions.
    * @param eq
    * @return
    */
  def ==(eq: EquivalenceClass[A]): Boolean = this.contains(eq.representative) && eq.contains(this.representative)

  def !=(eq: EquivalenceClass[A]): Boolean = !(this == eq)

  //TODO: Is this implementation really correct?
  override def equals(obj: scala.Any): Boolean = obj match {
    case eq: EquivalenceClass[A] => this == eq
    case _ => false
  }

}

object EquivalenceClass {

  /**
    * Creates an EquivalenceClass
    * @param a
    * @param f
    * @tparam A
    * @return
    */
  def apply[A](a: A, f: EquivalenceRelation[A]): EquivalenceClass[A] = {

    new EquivalenceClass[A] {
      override def representative = a

      override def relation = f
    }

  }
}
