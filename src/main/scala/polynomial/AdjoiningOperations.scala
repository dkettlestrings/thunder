package polynomial

import algebra.ring.CommutativeRing
import core.{EuclideanDomain, ExtendedInteger, Field}
import core.InfixOps._
import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Adds the ability to adjoin a [[FormalParameter]] to create a ring of [[Polynomial]]s.
  */
object AdjoiningOperations {

  implicit def toAdjoinableCommutativeRing[A](r: CommutativeRing[A]): AdjoinableCommutativeRing[A] = new AdjoinableCommutativeRing[A] {
    override def domain: CommutativeRing[A] = r
  }

  implicit def toAdjoinableField[A](f: Field[A]): AdjoinableField[A] = new AdjoinableField[A] {
    override def field: Field[A] = f
  }

  trait AdjoinableCommutativeRing[A] {

    def domain: CommutativeRing[A]

    /**
      * Create a CommutativeRing of [[Polynomial]]s with coefficients in the original [CommutativeRing.
      *
      * Use this operation on CommutativeRings.  See f_adjoin for operating on Fields.
      * @param p
      * @return
      */
    def r_adjoin(p: FormalParameter): CommutativeRing[Polynomial[A]]= new CommutativeRing[Polynomial[A]] with PolynomialRingOps[A] {

      override def coefficientRing: CommutativeRing[A] = domain

      override def param: FormalParameter = p

    }
  }

  trait AdjoinableField[A] {

    implicit def field: Field[A]

    /**
      * Create a EuclideanDomain of [[Polynomial]]s with coefficients in the original Field.
      *
      * Use this operation on Fields.  See r_adjoin for operating on CommutativeRings.
      * @param p
      * @return
      */
    def f_adjoin(p: FormalParameter): EuclideanDomain[Polynomial[A]] = new EuclideanDomain[Polynomial[A]] with PolynomialRingOps[A] {

      override def coefficientRing: CommutativeRing[A] = field

      override def param: FormalParameter = p

      private implicit val underlyingPolynomialRing = field r_adjoin p

      override def quot(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {

        @tailrec
        def go(a: Polynomial[A], b: Polynomial[A], acc: Polynomial[A]): Polynomial[A] = {
          if(a.degree < b.degree) acc
          else {
            val leadingCoefficient = field.div(a.leadingCoefficient, b.leadingCoefficient)
            val degreeDifference = a.degree - b.degree
            val xToTheN = Polynomial[A](param, field.one, field.zero) ^ degreeDifference.toInt // X^(degreeDifference)
            val newQuot = xToTheN * Polynomial[A](param, leadingCoefficient)
            val reducedA = a - (newQuot * b)
            go(reducedA, b, acc + newQuot)
          }
        }

        if(y == zero) throw new ArithmeticException("Dividing by zero")
        else go(x, y, zero)

      }

      override def mod(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = x - (quot(x, y) * y)

      override def norm(a: Polynomial[A]): ExtendedInteger = a.degree

    }
  }
}
