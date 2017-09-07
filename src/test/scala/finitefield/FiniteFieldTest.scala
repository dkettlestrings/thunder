package finitefield

import core.InfixOps._
import polynomial.Predef.X
import org.scalatest.{FunSuite, Matchers}
import polynomial.Polynomial
import core.InfixOps._

class FiniteFieldTest extends FunSuite with Matchers {

  test("A finite field with 3 elements should be Z/3Z") {

    implicit val ff = FiniteField(3, 1)
    ff.elements.size should be (3)
    ff.characteristic should be (3)

    val zero = ff.zero
    val one = ff.one
    val two = one + one

    ff.elements should be (Set(zero, one, two))
    ff.elements.foreach(x =>
      x + x + x should be (zero)
    )

    zero + zero should be (zero)
    one + one should be (two)
    one + two should be (zero)

    two * zero should be (zero)
    two * two should be (one)
  }

  test("A finite field of 9 elements should be constructable") {

    val p = 3
    val n = 2
    val pToTheN = List.fill(n)(p).product

    implicit val ff = FiniteField(p, n)
    ff.elements.size should be (pToTheN)
    ff.characteristic should be (p)

    implicit val polyRing = PolynomialRingOverIntegersModP(p, X)
    implicit val coefficientRing = polyRing.coefficients
    val zero = polyRing.coefficients.classOf(0)
    val one = polyRing.coefficients.classOf(1)
    val two = polyRing.coefficients.classOf(2)

    val a = ff.classOf(Polynomial(X, zero))
    val b = ff.classOf(Polynomial(X, one))
    val c = ff.classOf(Polynomial(X, two))
    val d = ff.classOf(Polynomial(X, one, zero))
    val e = ff.classOf(Polynomial(X, one, one))
    val f = ff.classOf(Polynomial(X, one, two))
    val g = ff.classOf(Polynomial(X, two, zero))
    val h = ff.classOf(Polynomial(X, two, one))
    val i = ff.classOf(Polynomial(X, two, two))

    val elements = Set(a, b, c, d, e, f, g, h, i)

    ff.elements should be (elements)

    ff.elements.foreach(x =>
      x + x + x should be (ff.zero)
    )

    ff.elements.foreach(x =>
      x + a should be (x)
    )

    ff.elements.foreach(x =>
      x * b should be (x)
    )

    ff.elements.filterNot(_ == a).foreach(x =>
      x.inv should be
    )

    c + i should be (h)
    c + c should be (b)
    e + f should be (g)

  }
}
