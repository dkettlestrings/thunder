package polynomial

trait FormalParameter {

  def name: String

  override def toString: String = name

}

object FormalParameter {
  def apply(string: String): FormalParameter = new FormalParameter {
    override def name: String = string
  }
}

object Predef {

  implicit val X = FormalParameter("X")
  implicit val Y = FormalParameter("Y")
  implicit val Z = FormalParameter("Z")
}
