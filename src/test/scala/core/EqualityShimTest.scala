package core

import org.scalatest.{FunSuite, Matchers}

class EqualityShimTest extends FunSuite with Matchers {

  test("EqualityShim uses equalz on objects of the same type") {

    val f1 = Foo(1, 3)
    val f2 = Foo(2, 2)
    val f3 = Foo(2, 3)

    f1 == f2 should be (true)
    f2 == f1 should be (true)

    f1 == f3 should be (false)
    f3 == f1 should be (false)

    f2 == f3 should be (false)
    f3 == f2 should be (false)


    f1 != f2 should be (false)
    f2 != f1 should be (false)

    f1 != f3 should be (true)
    f3 != f1 should be (true)

    f2 != f3 should be (true)
    f3 != f2 should be (true)
  }

  test("Foos should work in Sets") {


    val s1: Set[Foo] = Set(Foo(1, 2), Foo(2, 3))
    val s2: Set[Foo] = Set(Foo(2, 1), Foo(1, 4))

    assert(s1 == s2)
  }

  test("Foos should not work in sets") {

    val s1: Set[Foo] = Set(Foo(1, 3), Foo(2, 3))
    val s2: Set[Foo] = Set(Foo(2, 1), Foo(1, 4))

    assert(s1 != s2)
  }



  trait Foo extends EqualityShim[Foo] {

    val a: Int

    val b: Int

    override def equalz(other: Foo): Boolean = {

      a + b == other.a + other.b
    }

  }

  object Foo {

    def apply(x: Int, y: Int): Foo = new Foo {

      override val a: Int = x

      override val b: Int = y
    }
  }

}
