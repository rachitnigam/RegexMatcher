import RegexSyntax._
import RegexParser._

object RegexMatcher {
  
  def reduceSimple(r: RExpr): RExpr = r match {
    case RSeq(RNothing,_) => RNothing
    case RSeq(_,RNothing) => RNothing
    case RSum(RNothing,r1) => reduce(r1)
    case RSum(r1,RNothing) => reduce(r1)
    case RSeq(r1,REmpty) => reduce(r1)
    case RSeq(REmpty,r1) => reduce(r1)
    case RSum(r1,r2) => if (r1 == r2) reduce(r1) else r
    case _ => r
  }

  def reduce(r: RExpr): RExpr = {
    val reg = reduceSimple(r) 
    reg match {
    case RSeq(r1,r2) => {
      val p = reduceSimple(RSeq(reduce(r1),reduce(r2)))
      if(p == reg) reg else reduce(p)
    }
    case RSum(r1,r2) => {
      val p = reduceSimple(RSum(reduce(r1),reduce(r2)))
      if(p == reg) reg else reduce(p)
    }
    case _ => reg
  }
  }

  def nullOf(r: RExpr): RExpr = r match {
    case REmpty => r
    case RNothing => r
    case RLetter(_) => RNothing
    case RSeq(r1,r2) => reduce(RSeq(nullOf(r1),nullOf(r2)))
    case RSum(r1,r2) => reduce(RSum(nullOf(r1),nullOf(r2)))
    case RRep(_) => REmpty
  }

  def derivative(r: RExpr, c: Char): RExpr = r match {
    case REmpty | RNothing => RNothing
    case RLetter(l) => if(l == c) REmpty else RNothing
    case RSeq(r1,r2) => reduce(RSum(RSeq(derivative(r1,c),r2),RSeq(nullOf(r1),derivative(r2,c)))) 
    case RRep(l) => reduce(RSeq(derivative(RLetter(l),c),RRep(l)))
    case RSum(r1,r2) => reduce(RSum(derivative(r1,c),derivative(r2,c)))
  }
  
  def matches(reg: RExpr, str: String): Boolean = str match {
    case "" => nullOf(reg) == REmpty
    case s => {
      val d = derivative(reg,str.head)
      (d != RNothing) && matches(d,s.tail)
    }
  }

  def main(args: Array[String]): Unit = {
    val r = parse("a.(b* + c*)")
    println(matches(r,"a"))
  }
}
