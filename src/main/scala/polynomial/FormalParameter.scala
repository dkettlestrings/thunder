package polynomial

/**
  * A formal variable such as X, Y, Z, etc.
  *
  * A FormalParameter represents an unknown value (a variable) usually within the context of a [[Polynomial]].
  */
trait FormalParameter {

  def name: String

  override def toString: String = name

}

/**
  * Companion object for construction.
  */
object FormalParameter {
  def apply(string: String): FormalParameter = new FormalParameter {
    override def name: String = string
  }
}

/**
  * Implicit predefined [[FormalParameter]]s.
  */
object Predef {

  implicit val X = FormalParameter("X")
  implicit val Y = FormalParameter("Y")
  implicit val Z = FormalParameter("Z")
}
