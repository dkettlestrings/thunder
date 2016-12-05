package core

/**
  * The integers along with positive and negative infinity.
  */
sealed trait ExtendedInteger extends Ordered[ExtendedInteger] {

  def +(ei: ExtendedInteger): ExtendedInteger

  def -(ei: ExtendedInteger): ExtendedInteger

  //TODO: Allow for implicit conversion to ints
  def toInt: Int = this match {
    case FiniteInteger(a) => a
    case _ => throw new ArithmeticException("Cannot convert infinite values to Int")
  }

  override def compare(that: ExtendedInteger): Int = this match {
    case FiniteInteger(a) => that match {
      case FiniteInteger(b) => a.compare(b)
      case PositiveInfinity => -1
      case NegativeInfinity => 1
    }

    case PositiveInfinity => that match {
      case FiniteInteger(_) => 1
      case PositiveInfinity => 0
      case NegativeInfinity => 1
    }

    case NegativeInfinity => that match {
      case FiniteInteger(_) => -1
      case PositiveInfinity => -1
      case NegativeInfinity => 0
    }
  }

}

case class FiniteInteger(value: Int) extends ExtendedInteger {
  override def +(ei: ExtendedInteger): ExtendedInteger = ei match {
    case FiniteInteger(a) => FiniteInteger(value + a)
    case PositiveInfinity => PositiveInfinity
    case NegativeInfinity => NegativeInfinity
  }

  override def -(ei: ExtendedInteger): ExtendedInteger = ei match {
    case FiniteInteger(a) => FiniteInteger(value - a)
    case PositiveInfinity => NegativeInfinity
    case NegativeInfinity => PositiveInfinity
  }
}

case object PositiveInfinity extends ExtendedInteger {
  override def +(ei: ExtendedInteger): ExtendedInteger = ei match {
    case FiniteInteger(_) => PositiveInfinity
    case PositiveInfinity => PositiveInfinity
    case NegativeInfinity => throw new ArithmeticException("Positive infinity plus negative infinity is undefined")
  }

  override def -(ei: ExtendedInteger): ExtendedInteger = ei match {
    case FiniteInteger(_) => PositiveInfinity
    case PositiveInfinity => throw new ArithmeticException("Positive infinity minus positive infinity is undefined")
    case NegativeInfinity => PositiveInfinity
  }
}

case object NegativeInfinity extends ExtendedInteger {
  override def +(ei: ExtendedInteger): ExtendedInteger = ei match {
    case FiniteInteger(_) => NegativeInfinity
    case PositiveInfinity => throw new ArithmeticException("Negative infinity plus positive infinity is undefined")
    case NegativeInfinity => NegativeInfinity
  }

  override def -(ei: ExtendedInteger): ExtendedInteger = ei match {
    case FiniteInteger(_) => NegativeInfinity
    case PositiveInfinity => NegativeInfinity
    case NegativeInfinity => throw new ArithmeticException("Negative infinity minus negative infinity is undefined")
  }
}
