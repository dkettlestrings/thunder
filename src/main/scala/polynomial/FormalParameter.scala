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

  val X = FormalParameter("X")
  val Y = FormalParameter("Y")
  val Z = FormalParameter("Z")
}
