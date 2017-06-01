package core

import algebra.ring.CommutativeRing

/**
  * Created by dkettlestrings on 6/1/17.
  */
trait EuclideanDomain[A] extends CommutativeRing[A] {

  def norm(a: A): Double

  def quot(x: A, y: A): A

  def mod(x: A, y: A): A

}
