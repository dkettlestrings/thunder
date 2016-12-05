package polynomial

import algebra.ring.{CommutativeRing, EuclideanRing, Field}
import PolynomialOps._

import language.implicitConversions
import scala.annotation.tailrec

/**
  * Adds the ability to adjoning a FormalParameter to create a ring of Polynomials.
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
      * Create a CommutativeRing of Polynomials with coefficients in the original CommutativeRing.
      *
      * Use this operation on CommutativeRings.
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
      * Create a EuclideanRing of polynomials with coefficients in the original Field.
      *
      * Use this operation on Fields.
      * @param p
      * @return
      */
    def f_adjoin(p: FormalParameter): EuclideanRing[Polynomial[A]] = new EuclideanRing[Polynomial[A]] with PolynomialRingOps[A] {

      override def coefficientRing: CommutativeRing[A] = field

      override def param: FormalParameter = p

      implicit val underlyingPolynomialRing = field r_adjoin p
      implicit val ringOps = underlyingPolynomialRing.asInstanceOf[PolynomialRingOps[A]]

      override def quot(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = {

        @tailrec
        def go(a: Polynomial[A], b: Polynomial[A], acc: Polynomial[A]): Polynomial[A] = {
          if(a.degree < b.degree) acc
          else {
            val leadingCoefficient = field.div(a.leadingCoefficient, b.leadingCoefficient)
            val degreeDifference = a.degree - b.degree
            val xToTheN = polynomial(field.one, field.zero) ^ degreeDifference.toInt // X^(degreeDifference)
            val newQuot = xToTheN * polynomial(leadingCoefficient)
            val reducedA = a - (newQuot * b)
            go(reducedA, b, acc + newQuot)
          }
        }

        if(y == zero) throw new ArithmeticException("Dividing by zero")
        else go(x, y, zero)

      }

      override def mod(x: Polynomial[A], y: Polynomial[A]): Polynomial[A] = x - (quot(x, y) * y)

    }
  }
}
