package core

trait EqualityShim[A] {

  override def equals(obj: scala.Any): Boolean = {

    try {

      val asA: A = obj.asInstanceOf[A]
      this equalz asA

    } catch {
      case _ : ClassCastException => false
    }

  }

  def equalz(other: A): Boolean

}
