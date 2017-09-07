package core

trait SemiFinite[A] {

  def elementsUpTo(a: A): Set[A]
  
}
