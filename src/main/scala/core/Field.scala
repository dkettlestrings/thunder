package core

/**
  * Created by dkettlestrings on 6/1/17.
  */
trait Field[A] extends EuclideanDomain[A] {

  override def norm(a: A): ExtendedInteger = a match {

    case x if x == zero => NegativeInfinity
    case _ => FiniteInteger(1)
  }

  override def quot(x: A, y: A): A = y match {

    case x if x == zero => throw new ArithmeticException("Dividing by zero")
    case _ => one
  }

  override def mod(x: A, y: A): A = y match {

    case x if x == zero => throw new ArithmeticException("Dividing by zero")
    case _ => one
  }

  def div(x: A, y: A): A

}
