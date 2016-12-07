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
