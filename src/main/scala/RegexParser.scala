package regex
import scala.util.parsing.combinator._

/*
 * r :=   w
 *      | w*
 *      | r1 + r2
 *      | r1.r2
 */

private class RParser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]
  import RegexSyntax._

  lazy val parseWord: P[Char] = "[a-zA-Z0-9]".r ^^ {case s => s.head} 
  lazy val parseLetter: P[RExpr] = parseWord ^^ {case w => RLetter(w)}
  
  lazy val parseAtom = parseLetter | "(" ~> parseSum <~ ")"

  lazy val parseRep: P[RExpr] = parseAtom <~ "*" ^^ {case w => RRep(w)} | parseAtom
  lazy val parseSeq: P[RExpr] = parseSeq ~ "." ~ parseAtom ^^ {case r1 ~ _ ~ r2 => r1*r2} | parseRep
  lazy val parseSum: P[RExpr] = parseSum ~ "+" ~ parseSeq ^^ {case r1 ~ _ ~ r2 => r1+r2} | parseSeq

  def parse(str: String): RExpr = parseAll(parseSum,str) match {
    case Success(s,_) => s
    case NoSuccess(msg,_) => throw new Exception(msg)
  }

}


object RegexParser {
  import RegexSyntax._
  private val parser = new RParser()
  
  def parse = parser.parse _
}
