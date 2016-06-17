package regex
object RegexSyntax {

  sealed trait RExpr {
    def +(that: RExpr): RExpr = (this,that) match {
      case (RNothing,r1) => r1
      case (r1,RNothing) => r1
      case (r1,r2) => RSum(r1,r2)
    }
    def *(that: RExpr): RExpr = (this,that) match {
      case (RNothing,_) => RNothing
      case (_,RNothing) => RNothing
      case (REmpty,r) => r
      case (r,REmpty) => r
      case (r1,r2) => RSeq(r1,r2)
    }
  }
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
  case class RRep(r: RExpr) extends RExpr{
    override def toString() = s"$r*"
  }
}
