package core

/**
  * The set of all elements of type A that are equivalent under some EquivalenceRelation.
  *
  * Given a particular [[EquivalenceRelation]], the set of EquivalenceClasses form a partition for the elements of A.
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

  /**
    * Determines whether the EquivalenceClasses are equal.
    *
    * This is done by checking whether each EquivalenceClass contains the representative element of the other.  Note
    * that this check is somewhat unsafe since it does not check whether the two [[EquivalenceRelation]]s are the same.
    * This shortcoming is due to the fact that it would require equality checks for functions.  This shortcoming is
    * overcome in certain subtypes such as [[ResidueClass]].
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

/**
  * Companion object for construction.
  */
object EquivalenceClass {

  /**
    * Creates an [[EquivalenceClass]]
    * @param representativeElement
    * @param equivalenceRelation
    * @tparam A
    * @return
    */
  def apply[A](representativeElement: A, equivalenceRelation: EquivalenceRelation[A]): EquivalenceClass[A] = {

    new EquivalenceClass[A] {
      override def representative = representativeElement

      override def relation = equivalenceRelation
    }

  }
}
