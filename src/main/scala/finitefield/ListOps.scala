package finitefield

//TODO: Get rid of this stupid thing!  See https://github.com/dkettlestrings/thunder/issues/61
object ListOps {

  def product[A](input: List[A], size: Int): Set[List[A]] = {

    require(input.nonEmpty)
    require(size >= 0)

    size match {

      case 0 => Set(List.empty)
      case s if s > 0 => input.flatMap(a => product(input, s - 1).map(x => x :+ a)).toSet
    }



  }


}
