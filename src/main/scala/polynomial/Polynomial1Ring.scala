package polynomial

import algebra.ring.CommutativeRing

//TODO: Make this a Euclidean Ring so that we can effectively make rational functions
trait Polynomial1Ring[A] extends CommutativeRing[Polynomial1[A]] {

  def param: FormalParameter

  def ring: CommutativeRing[A]

  //TODO: Make this pretty!
  //TODO: For the zero case, return negative infinity!
  def degree(polynomial: Polynomial1[A]): Int = {
    val exponentsWithNonZeroCoefficients = polynomial.coefficients.filter(_._2 != ring.zero).keySet
    if(exponentsWithNonZeroCoefficients.size == 0) 0
    else exponentsWithNonZeroCoefficients.max
  }

  def leadingCoefficient(polynomial: Polynomial1[A]): A = polynomial.coefficients.getOrElse(this.degree(polynomial), ring.zero)

  def polynomial(xa: A*): Polynomial1[A] = {

    val exponentMap = xa.zipWithIndex.toMap.map({case (a, i) => (xa.size - i - 1, a)})
    Polynomial1(param, exponentMap)
  }

  override def plus(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = Polynomial1(param, mapPlus(x.coefficients, y.coefficients))

  override def minus(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = Polynomial1(param, mapMinus(x.coefficients, y.coefficients))

  override def one: Polynomial1[A] = Polynomial1(param, Map(0 -> ring.one))

  override def zero: Polynomial1[A] = Polynomial1(param, Map(0 -> ring.zero))

  override def negate(x: Polynomial1[A]): Polynomial1[A] = Polynomial1(param, mapNegate(x.coefficients))

  override def times(x: Polynomial1[A], y: Polynomial1[A]): Polynomial1[A] = Polynomial1(param, mapTimes(x.coefficients, y.coefficients))

  private def mapPlus(x: Map[Int, A], y: Map[Int, A]): Map[Int, A] = binaryMapOp(ring.plus, ring.zero)(x, y)

  private def mapMinus(x: Map[Int, A], y: Map[Int, A]): Map[Int, A] = binaryMapOp(ring.minus, ring.zero)(x, y)

  private def mapNegate(x: Map[Int, A]): Map[Int, A] = x.mapValues(coef => ring.negate(coef))

  private def mapTimes(x: Map[Int, A], y: Map[Int, A]): Map[Int, A] = binaryMapOp(ring.times, ring.one)(x, y)

  private def binaryMapOp(op: (A, A) => A, unit: A)(x: Map[Int, A], y: Map[Int, A]): Map[Int, A] = {

    (x.keySet ++ y.keySet).map(exp => exp -> op(x.getOrElse(exp, unit), y.getOrElse(exp, unit))).toMap
  }
}

object Polynomial1Ring {
  def apply[A](p: FormalParameter, r: CommutativeRing[A]): Polynomial1Ring[A] = new Polynomial1Ring[A] {

    override def param: FormalParameter = p

    override def ring: CommutativeRing[A] = r

  }
}

trait Polynomial1[A] {

  def param: FormalParameter

  def coefficients: Map[Int, A]

}

object Polynomial1 {
  def apply[A](variable: FormalParameter, coef: Map[Int, A]) = {
    new Polynomial1[A] {
      override def param: FormalParameter = variable

      override def coefficients: Map[Int, A] = coef

    }
  }
}