package core

//TODO: Find a better home for integers.  See https://github.com/dkettlestrings/thunder/issues/48
/**
  * Convenience methods for the integers modulo n.
  */
object IntegerModding {

  /**
    * The EuclideanRing of integers.
    * @return
    */
  implicit def integers = Integers()

}
