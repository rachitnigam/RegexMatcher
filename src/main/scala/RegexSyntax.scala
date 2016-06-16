object RegexSyntax {

  sealed trait RExpr
  case object RNothing extends RExpr {
    override def toString()  = "nothing"
  }
  case object REmpty extends RExpr {
    override def toString(): String = "empty"
  }
  case class RLetter(letter: Char) extends RExpr {
    override def toString() = letter.toString
  }
  case class RSum(r1: RExpr, r2: RExpr) extends RExpr {
    override def toString() = s"($r1 + $r2)"
  }
  case class RSeq(r1: RExpr, r2: RExpr) extends RExpr {
    override def toString() = s"($r1.$r2)"
  }
  case class RRep(letter: Char) extends RExpr{
    override def toString() = s"$letter*"
  }
}
