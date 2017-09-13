package polynomial

import algebra.ring.CommutativeRing
import core.{EqualityShim, ExtendedInteger, FiniteInteger, NegativeInfinity}

trait Polynomial2[A] extends EqualityShim[Polynomial2[A]] {

  def param1: FormalParameter

  def param2: FormalParameter

  def coefficients: Map[(Int, Int), A]

  implicit val coefficientRing: CommutativeRing[A]

  //TODO: change this from Option to just returning zero!
  def coefficient: (Int, Int) => Option[A] = (d1, d2) => if (coefficients.isDefinedAt(d1, d2)) Some(coefficients(d1, d2)) else None

  def degree: ExtendedInteger = {

    val trimmed = trimZeroCoefficients(coefficients)
    if(trimmed.isEmpty) NegativeInfinity
    else {
      val sumOfDegrees = trimmed.map({case (key, _) =>
        val (d1, d2) = key
        d1 + d2}).max
      FiniteInteger(sumOfDegrees)
    }
  }

  override def equalz(other: Polynomial2[A]): Boolean = {
    param1 == other.param1 &&
    param2 == other.param2 &&
    trimZeroCoefficients(coefficients) == trimZeroCoefficients(other.coefficients)
  }

  override def hashCode(): Int = param1.hashCode + param2.hashCode + trimZeroCoefficients(coefficients).hashCode

  private def trimZeroCoefficients(map: Map[(Int, Int), A]): Map[(Int, Int), A] = {

    map.filterNot({case (key, _) => map(key) == coefficientRing.zero})
  }

}

object Polynomial2 {

  def apply[A](p1: FormalParameter, p2: FormalParameter, coef: Map[(Int, Int), A])(implicit ring: CommutativeRing[A]): Polynomial2[A] = {

    require(coef.forall({case (key, _) => key._1 >= 0 && key._2 >= 0}))

    new Polynomial2[A] {


      override val param1 = p1
      override val param2 = p2
      override val coefficients = coef
      override implicit val coefficientRing = ring

    }
  }
}
