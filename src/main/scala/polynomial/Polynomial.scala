package polynomial

import algebra.ring.CommutativeRing

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

//TODO: coefficient vs. coefficients is confusing. See https://github.com/dkettlestrings/thunder/issues/45
/**
  * Just a polynomial.
  *
  * @tparam A The type of the coefficients.
  */
trait Polynomial[A] {

  def param: FormalParameter

  // coefficients are stored where index i corresponds to coefficient for the degree i term
  //TODO: Don't tie this trait to a particular implementation.  Use PartialFunction instead.  See https://github.com/dkettlestrings/thunder/issues/45
  /**
    * A list of all the coefficients for the polynomial.
    *
    * Note that the element at index i corresponds to the coefficient on the ith power of the parameter.  Note also that
    * this method is very easy to confuse with coefficient and is possibly redundant.  As such it may be deprecated, see
    * https://github.com/dkettlestrings/thunder/issues/45.
    *
    * @return
    */
  def coefficients: List[A]

  def coefficientRing: CommutativeRing[A]

  /**
    * Gives the coefficient associated with the term of degree d if it exists.
    *
    * Note that this method is very easy to confuse with coefficients and is possibly redundant.  As such it may be
    * deprecated, see https://github.com/dkettlestrings/thunder/issues/45.
    *
    * @param d
    * @return
    */
  def coefficient(d: Int): Option[A] = if (coefficients.isDefinedAt(d)) Some(coefficients(d)) else None

  /**
    * Returns the degree of the polynomial.
    *
    * Note that by convention, the degree of the zero polynomial is [[NegativeInfinity]].  This is the only polynomial
    * with this degree.
    *
    * @return
    */
  def degree: ExtendedInteger = {
    biggestKeyWithNonzeroValue(coefficients) match {
      case Some(a) => FiniteInteger(a)
      case None => NegativeInfinity
    }
  }

  // The @unchecked is to suppress a compiler warning for not matching against PositiveInfinity, which is impossible
  /**
    * Returns the coefficient attached to the highest power term in the polynomial.
    *
    * This will return the coefficient attached to the highest-power term in the polynomial.  If there is no such term
    * (as in the case of the zero polynomial) the zero from the coefficient ring will be returned.  As such, the
    * leading coefficient is the zero element from the coefficient ring if and only if the polynomial is equal to the
    * zero polynomial.
    *
    * @param coefficientRing
    * @return
    */
  def leadingCoefficient(implicit coefficientRing: CommutativeRing[A]): A = (degree: @unchecked) match {
    case NegativeInfinity => coefficientRing.zero
    case FiniteInteger(a) => coefficients(a)
  }

  //TODO: Create some kind of Equalable trait to wrap the type checking.  This would be used in many places.  See https://github.com/dkettlestrings/thunder/issues/46
  override def equals(obj: scala.Any): Boolean = {

    Try(obj.asInstanceOf[Polynomial[A]]) match {
      case Success(poly) => this.param == poly.param && trimLeadingZeros(this.coefficients) == trimLeadingZeros(poly.coefficients)
      case Failure(throwable) => false
    }
  }

  override def hashCode(): Int = param.hashCode() + trimLeadingZeros(coefficients).hashCode

  /**
    * Returns a String representation of the polynomial in the common mathematical form.
    *
    * This method follows the mathematical standards of
    *
    * 1. Writing the terms in descending order by degree (from left to right)
    *
    * 2. Removing any terms with zero coefficients (see exception below)
    *
    * 3. In the case of the zero polynomial, the result will be displayed as the constant zero
    *
    * 4. Terms with degree zero do not have the parameter written
    *
    * 5. Terms of degree one have no exponent written
    *
    * @return
    */
  override def toString: String = {

    if (coefficients.forall(_ == coefficientRing.zero)) coefficientRing.zero.toString
    else {

      val nonZeroTerms = (coefficients zipWithIndex).filter {case (c, _) => c != coefficientRing.zero}

      val termStrings: List[String] = nonZeroTerms map {case (c, d) =>
        val parameterString = d match {
          case 0 => ""
          case 1 => s"$param"
          case _ => s"$param^$d"
        }
        c.toString + parameterString
      }

      termStrings.reverse.mkString(" + ")
    }

  }

  def +(other: Polynomial[A])(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.plus(this, other)

  def -(other: Polynomial[A])(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.minus(this, other)

  def *(other: Polynomial[A])(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.times(this, other)

  /**
    * Exponentiation (repeated multiplication) operator.
    *
    * Note that this implementation adopts the convention of if z is the zero element, z^0 == 0.
    *
    * @param exp
    * @param ring
    * @return
    */
  def ^(exp: Int)(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.pow(this, exp)

  def negate(implicit ring: CommutativeRing[Polynomial[A]]): Polynomial[A] = ring.negate(this)

  @tailrec
  private def trimLeadingZeros(a: List[A]): List[A] = a match {
    case Nil => a
    case _ :+ last if last == coefficientRing.zero => trimLeadingZeros(a.init)
    case _ => a
  }

  private def biggestKeyWithNonzeroValue(a: List[A]): Option[Int] = {
    trimLeadingZeros(a).size match {
      case 0 => None
      case n => Some(n - 1)
    }
  }

}

/**
  * Companion object
  */
object Polynomial {

  /**
    * Constructs a polynomial.
    *
    * The constructor follows the mathematical convention of writing coefficients from left to right in decreasing
    * order (by degree).  In other words, the first coefficient will have the highest power and so forth.  The last
    * coefficient is assumed to be the constant term (degree zero).

    * @param p
    * @param xa
    * @param ring
    * @tparam A
    * @return
    */
  def apply[A](p: FormalParameter, xa: List[A])(implicit ring: CommutativeRing[A]): Polynomial[A] = new Polynomial[A] {

    override def param: FormalParameter = p

    override def coefficients: List[A] = xa.reverse  //TODO: Document this!!!

    override def coefficientRing: CommutativeRing[A] = ring
  }

  /**
    * Constructs a polynomial.
    *
    * The constructor follows the mathematical convention of writing coefficients from left to right in decreasing
    * order (by degree).  In other words, the first coefficient will have the highest power and so forth.  The last
    * coefficient is assumed to be the constant term (degree zero).
    *
    * @param p
    * @param xa
    * @param ring
    * @tparam A
    * @return
    */
  def apply[A](p: FormalParameter, xa: A*)(implicit ring: CommutativeRing[A]): Polynomial[A] = {

    Polynomial[A](p, xa.toList)
  }

  /**
    * Constructs a polynomial.
    *
    * The constructor follows the mathematical convention of writing coefficients from left to right in decreasing
    * order (by degree).  In other words, the first coefficient will have the highest power and so forth.  The last
    * coefficient is assumed to be the constant term (degree zero).
    *
    * @param xa
    * @param p
    * @param ring
    * @tparam A
    * @return
    */
  def apply[A](xa: A*)(implicit p: FormalParameter, ring: CommutativeRing[A]): Polynomial[A] = {

    Polynomial[A](p, xa.toList)
  }
}