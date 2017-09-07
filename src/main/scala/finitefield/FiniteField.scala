package finitefield

import core.ModuloOperations._
import core.{Finite, Field, ResidueClass}
import integer.IntegersModP
import polynomial.Polynomial
import polynomial.Predef.X

import core.InfixOps._

import scala.language.postfixOps

trait FiniteField extends Field[ResidueClass[Polynomial[ResidueClass[Int]]]] with Finite[ResidueClass[Polynomial[ResidueClass[Int]]]] {

  def characteristic: Int

}

object FiniteField {

  def apply(p: Int, exp: Int): FiniteField = {

    implicit val primeField = IntegersModP(p)
    implicit val polyRing = PolynomialRingOverIntegersModP(p, X)
    implicit def context = PolynomialOverIntsModPModdingContext(polyRing)

    //TODO: look up polynomial to use!  Also, create a DSL for making this prettier.  See https://github.com/dkettlestrings/thunder/issues/44
    val irreducible = (Polynomial(polyRing.coefficients.one, polyRing.coefficients.zero) ^ exp ) - polyRing.one

    implicit val delegate = polyRing modulo_f irreducible

    new FiniteField {

      override def characteristic: Int = p

      override def elements: Set[ResidueClass[Polynomial[ResidueClass[Int]]]] = context.elementsUpTo(irreducible).map(poly => ResidueClass(poly, irreducible)).toSet

      override def zero: ResidueClass[Polynomial[ResidueClass[Int]]] = delegate.zero

      override def one: ResidueClass[Polynomial[ResidueClass[Int]]] = delegate.one

      override def plus(x: ResidueClass[Polynomial[ResidueClass[Int]]], y: ResidueClass[Polynomial[ResidueClass[Int]]]): ResidueClass[Polynomial[ResidueClass[Int]]] = x + y

      override def negate(x: ResidueClass[Polynomial[ResidueClass[Int]]]): ResidueClass[Polynomial[ResidueClass[Int]]] = x.negate

      override def times(x: ResidueClass[Polynomial[ResidueClass[Int]]], y: ResidueClass[Polynomial[ResidueClass[Int]]]): ResidueClass[Polynomial[ResidueClass[Int]]] = x * y

      override def div(x: ResidueClass[Polynomial[ResidueClass[Int]]], y: ResidueClass[Polynomial[ResidueClass[Int]]]): ResidueClass[Polynomial[ResidueClass[Int]]] = x / y

    }
  }
}
