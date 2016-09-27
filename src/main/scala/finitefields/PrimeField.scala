package finitefields

import algebra.ring.Field

//TODO: Ask non to make default implementations of quot and mod in Field
//TODO: Is there a better exception available than ArithmeticException?
trait PrimeField extends IntegersMod with Field[ResidueClass] {

  override def quot(x: ResidueClass, y: ResidueClass): ResidueClass = {
    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else if(x == zero) zero
    else one
  }

  override def mod(x: ResidueClass, y: ResidueClass): ResidueClass = {
    if(y == zero) throw new ArithmeticException("Dividing by zero")
    else zero
  }

  //TODO: Use the Extended Euclidean Algorithm
  def inv(x: ResidueClass): ResidueClass = {
    if(x == zero) throw new ArithmeticException("Dividing by zero")
    else {
      val possibleMods = (1 until modulus)
      val asResClasses = possibleMods map residueClass
      val timesInput = asResClasses map (times(x, _))
      val association = asResClasses zip timesInput
      val found = association find (_._2 == one)
      val res = found.get
      val invrse = res._1
      invrse
    }
  }

  override def div(x: ResidueClass, y: ResidueClass): ResidueClass = times(x, inv(y))

}

object PrimeField {

  def apply(p: Int, checkPrimality: Boolean = false): PrimeField = {

    if (checkPrimality) require(isPrime(p))

    new PrimeField {

      override def modulus: Int = p

    }

  }
}

//TODO: A better primality check
object isPrime {
  def apply(n: Int): Boolean = {

    if(n < 2) false
    else if(n == 2) true
    else {
      val bound = scala.math.sqrt(n.toDouble).toInt + 1
      !(2 to bound).map(divisor => n % divisor).contains(0)
    }

  }
}