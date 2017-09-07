package core

/**
  * A simple trait that indicates that instances "contain" a finite number of elements.
  * @tparam A
  */
trait Finite[A] {

  def elements: Set[A]

}
