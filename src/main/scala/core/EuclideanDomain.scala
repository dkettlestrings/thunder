package core

import algebra.ring.CommutativeRing

/**
  *
  * @tparam A
  */
trait EuclideanDomain[A] extends CommutativeRing[A] {

  def norm(a: A): ExtendedInteger

  def quot(x: A, y: A): A

  def mod(x: A, y: A): A

}
