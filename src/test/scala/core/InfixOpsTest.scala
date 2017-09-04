package core

import algebra.ring.CommutativeRing
import InfixOps._
import org.scalatest.{FunSuite, Matchers}

class InfixOpsTest extends FunSuite with Matchers {

  test("An implicit field allows for infix operations") {

    case class Frac(num: Double, denom: Double)

    implicit val field: Field[Frac] = new Field[Frac] {

      override val zero = Frac(0, 1)
      override val one = Frac(1, 1)
      override def plus(x: Frac, y: Frac) = Frac(x.num * y.denom + x.denom * y.num, x.denom * y.denom)
      override def times(x: Frac, y: Frac) = Frac(x.num * y.num, x.denom * y.denom)
      override def negate(x: Frac) = Frac(-1 * x.num, x.denom)
      override def div(x: Frac, y: Frac) = Frac(x.num * y.denom, x.denom * y.num)
    }

    val a = Frac(1, 2)
    val b = Frac(2, 3)

    a + b should be
    a - b should be
    a * b should be
    a ^ 4 should be
    a.negate should be
    a / b should be
    a.inv should be
  }

  test("An implicit Euclidean domain allows for infix operations") {

    case class MyInt(value: Int)

    implicit val domain: EuclideanDomain[MyInt] = new EuclideanDomain[MyInt] {

      override def mod(x: MyInt, y: MyInt): MyInt = MyInt(x.value & y.value)
      override def norm(a: MyInt): ExtendedInteger = FiniteInteger(a.value)
      override def quot(x: MyInt, y: MyInt): MyInt = MyInt(x.value / y.value)
      override def zero: MyInt = MyInt(0)
      override def one: MyInt = MyInt(1)
      override def times(x: MyInt, y: MyInt): MyInt = MyInt(x.value * y.value)
      override def negate(x: MyInt): MyInt = MyInt(-1 * x.value)
      override def plus(x: MyInt, y: MyInt): MyInt = MyInt(x.value + y.value)
    }

    val a = MyInt(4)
    val b = MyInt(6)

    a + b should be
    a - b should be
    a * b should be
    a ^ 4 should be
    a.negate should be
    a mod b should be
    a quot b should be
    a.norm should be
  }

  test("An implicit commutative ring allows for infix operations") {

    case class Int_(value: Int)

    implicit val ring: CommutativeRing[Int_] = new CommutativeRing[Int_] {

      override val zero = Int_(0)
      override val one = Int_(1)
      override def plus(x: Int_, y: Int_): Int_ = Int_(x.value + y.value)
      override def times(x: Int_, y: Int_): Int_ = Int_(x.value * y.value)
      override def negate(x: Int_): Int_ = Int_(-1 * x.value)
    }

    val a = Int_(4)
    val b = Int_(6)

    a + b should be
    a - b should be
    a * b should be
    a ^ 4 should be
    a.negate should be
  }

}
