package rationalfunction

import polynomial.{FormalParameter, Polynomial1}

trait RationalFunction1[A] {

  def param: FormalParameter

  def numerator: Polynomial1[A]

  def denominator: Polynomial1[A]

}

object RationalFunction1 {

  def apply[A](variable: FormalParameter, top: Polynomial1[A], bottom: Polynomial1[A]) = new RationalFunction1[A] {

    override def param: FormalParameter = variable

    override def numerator: Polynomial1[A] = top

    override def denominator: Polynomial1[A] = bottom
  }
}
