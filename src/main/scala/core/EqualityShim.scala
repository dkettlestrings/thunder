package core

/**
  * A horrible workaround to address the limitations of the JRE and the Scala type system.  The idea is that you
  * can define an "equals" with a reasonable method signature (only accepts elements of the same type).  Given the
  * limitations of the JRE, you can still try to equate elements of different types and get runtime errors.
  *
  * The equalz method must satisfy the following three properties for all a, b, c in A:
  *
  * 1. a equalz a (reflexive property)
  *
  * 2. If a equalz b then b equalz a (symmetric property)
  *
  * 3. If a equalz b && b equalz c then a equalz c (transitive property)
  *
  * To use, simply override the equalz(other: A) method.  DO NOT override the builtin equals(a: Any) yourself.  Also,
  * it is up to the user to implement a hashcode() method that agrees with your notion of equality.
  * @tparam A
  */
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
