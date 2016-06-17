import regex._
import RegexParser._
import RegexSyntax._
import org.scalatest.FunSuite

class ParserTests extends FunSuite {

  test("Letter parsing") {
    assert(parse("a") equals RLetter('a'))
  }

  test("Sum parsing") {
    val s1 = parse("a + b")
    val s2 = parse("a") + parse("b")
    assert(s1 equals s2)
  }

  test("Sequence parsing") {
    val s1 = parse("a.b")
    val s2 = parse("a") * parse("b")
    assert(s1 equals s2)
  }

  test("Rep parsing") {
    val r1 = parse("a*")
    val r2 = RRep(RLetter('a'))
    assert(r1 equals r2)
  }

  test("Sum-seq parsing") {
    val r1 = parse("a.b + a.c")
    val r2 = parse("a.b") + parse("a.c")
    assert(r1 equals r2)
  }

  test("Sum-rep parsing") {
    val s1 = parse("a* + b*")
    val s2 = parse("a*") + parse("b*")
    assert(s1 equals s2)
  }

  test("Seq-rep parsing") {
    val s1 = parse("a.b*")
    val s2 = parse("a") * parse("b*")
    assert(s1 equals s2)
  }

  test("Rep-sum parsing") {
    val s1 = parse("(a+b)*")
    val s2 = RRep(parse("a") + parse("b"))
    assert(s1 equals s2)
  }

  test("Complex parse") {
    val s1 = parse("(a+b)*.(c+d)* + (e*+f*)")
    val s2 = (RRep(parse("a") + parse("b")) * RRep(parse("c") + parse("d"))) + (RRep(parse("e")) + RRep(parse("f")))
    assert(s1 equals s2)
  }
}
