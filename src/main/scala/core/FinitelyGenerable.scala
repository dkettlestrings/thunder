package core

/**
  * A simple trait that indicates that instances "contain" a finite number of elements.
  * @tparam A
  */
trait FinitelyGenerable[A] {

  def elements: Set[A]

}
