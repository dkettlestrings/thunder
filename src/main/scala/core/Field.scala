package core

/**
  * Created by dkettlestrings on 6/1/17.
  */
trait Field[A] extends EuclideanDomain[A] {

  override def norm(a: A): ExtendedInteger = a match {

    case x if x == zero => NegativeInfinity
    case _ => FiniteInteger(1)
  }

  override def quot(x: A, y: A): A = div(x, y)

  override def mod(x: A, y: A): A = y match {

    case z if z == zero => throw new ArithmeticException("Dividing by zero")
    case _ => zero
  }

  def div(x: A, y: A): A

}
