package polynomial

import algebra.ring.CommutativeRing

// coefficients(i) is the coefficient for the i_th degree term
case class Polynomial[A](param: FormalParameter, coefficients: List[A]) {

  def +(other: Polynomial[A])(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.plus(this, other)

  def -(other: Polynomial[A])(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.minus(this, other)

  def *(other: Polynomial[A])(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.times(this, other)

  def ^(exp: Int)(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.pow(this, exp)

  def negate(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.negate(this)

}
