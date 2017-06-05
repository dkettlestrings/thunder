package finitefield

import finitefield.ListOps.product
import org.scalatest.{FunSuite, Matchers}

class TestListOps extends FunSuite with Matchers {

  test("List of length zero throws exception") {

    intercept[IllegalArgumentException](product(List.empty[Int], 2))

  }

  test("Negative size throws exception") {

    intercept[IllegalArgumentException](product(List(1), -1))

  }

  test("Size 0 returns an empty List[List[_]]") {

    product(List(1), 0) should be (Set(List.empty))
    product(List(1, 2), 0) should be (Set(List.empty))

  }

  test("Size 1 works") {

    product(List(1), 1) should be (Set(List(1)))
    product(List(1, 2), 1) should be (Set(List(1), List(2)))
    product(List(1, 2, 3), 1) should be (Set(List(1), List(2), List(3)))

  }

  test("Lists of length 1 work") {

    product(List(1), 1) should be (Set(List(1)))
    product(List(1), 2) should be (Set(List(1, 1)))
    product(List(1), 3) should be (Set(List(1, 1, 1)))


  }

  test("Typical cases work") {

    product(List(1, 2, 3), 2) should be (Set(
      List(1, 1), List(1, 2), List(1, 3),
      List(2, 1), List(2, 2), List(2, 3),
      List(3, 1), List(3, 2), List(3, 3))
    )

  }

}
