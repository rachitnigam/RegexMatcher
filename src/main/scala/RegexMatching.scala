package regex

import RegexSyntax._
import RegexParser._

object RegexMatcher {
  
  def nullOf(r: RExpr): RExpr = r match {
    case REmpty => r
    case RNothing => r
    case RLetter(_) => RNothing
    case RSeq(r1,r2) => (nullOf(r1) * nullOf(r2))
    case RSum(r1,r2) => (nullOf(r1) + nullOf(r2))
    case RRep(_) => REmpty
  }

  def derivative(r: RExpr, c: Char): RExpr = r match {
    case REmpty | RNothing => RNothing
    case RLetter(l) => if(l == c) REmpty else RNothing
    case RSeq(r1,r2) => derivative(r1,c)*r2 + nullOf(r1)*derivative(r2,c) 
    case RRep(l) => derivative(l,c) * RRep(l) 
    case RSum(r1,r2) => derivative(r1,c) + derivative(r2,c) 
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
    println(derivative(r,"a".head))
  }
}
