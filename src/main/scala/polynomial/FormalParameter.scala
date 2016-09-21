package polynomial

case class FormalParameter(name: String) {

  override def toString: String = name
}

object Predef {

  val X = FormalParameter("X")
  val Y = FormalParameter("Y")
  val Z = FormalParameter("Z")
}
