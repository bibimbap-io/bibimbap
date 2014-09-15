package io.bibimbap.tests
package latex

import io.bibimbap.latex._

import org.scalatest.FunSuite
import org.scalatest.Matchers

class LaTeXParsing extends FunSuite with Matchers {
  private def mkParser = new LaTeXParser with LaTeXElements

  private val parser = mkParser

  import parser._

    // Tree equality, up to Seq types.
  private def compareTrees(t1 : LaTeXElement, t2 : LaTeXElement) : Unit = (t1, t2) match {
    case (Group(es1), Group(es2))         => compareTreeSeqs(es1, es2)
    case (RawString(v1), RawString(v2))   => assert(v1 === v2)
    case (Macro(n1, as1), Macro(n2, as2)) => assert(n1 === n2); compareTreeSeqs(as1, as2)
    case (Whitespace(v1), Whitespace(v2)) => assert(v1 === v2)
    case (MathMode(es1), MathMode(es2))   => compareTreeSeqs(es1, es2)
    case _                                => assert(t1 === t2) // That's going to fail...
  }

  private def compareTreeSeqs(s1 : LaTeXString, s2 : LaTeXString) : Unit = {
    assert(s1.length === s2.length,
          "LaTeX element sequences should have the same length.\nGot:\n    t%s\nand\n    %s)".format(
            s1.mkString("[", ", ", "]"),
            s2.mkString("[", ", ", "]")))

    for((e1,e2) <- s1 zip s2) {
      compareTrees(e1, e2)
    }
  }

  private def g(es : LaTeXElement*) = Group(es)
  private def m(es : LaTeXElement*) = MathMode(es)
  private def s(str : String) = RawString(str)
  private def w = Whitespace(" ")
  private def ws(s : String) = Whitespace(s)

  test("LaTeX normalization") {
    def checkNormalized(in : LaTeXElement*)(out : LaTeXElement*) = compareTreeSeqs(parser.normalize(in), out)

    checkNormalized(s("Hel"), s("lo"))(s("Hello"))
    checkNormalized(ws("\t"), ws(" "), ws("\n"))(ws("\t \n"))
    checkNormalized(g(g(s("Hello"))))(g(s("Hello")))
    checkNormalized(m(Macro("pi"), w, s("+"), m(Macro("rho"))))(m(Macro("pi"), w, s("+"), Macro("rho")))
  }

  test("Basic LaTeX parsing") {
    def checkParse(src : String, desired : LaTeXElement*) = compareTreeSeqs(parser.parse(src), desired) 

    checkParse("")
    checkParse("Hello", s("Hello"))
    checkParse("Hello world", s("Hello"), w, s("world"))
    checkParse("""\today""", Macro("today"))
    checkParse("""\frac{1}{2}""", Macro("frac", Seq(g(s("1")), g(s("2")))))
    checkParse("""\frac {1}{2}""", Macro("frac"), w, g(s("1")), g(s("2")))
    checkParse("""Consider $x \neq y$.""", s("Consider"), w, m(s("x"), w, Macro("neq"), w, s("y")), s("."))
    checkParse("""Vercing{\'{e}}torix""", s("Vercing"), g(Macro("'", Seq(g(s("e"))))), s("torix"))
    checkParse("""F\"ahndrich""", s("F"), Macro("\"a"), s("hndrich"))
    checkParse("""Fra\"{\i}ss\'e""", s("Fra"), Macro("\"", Seq(g(Macro("i")))), s("ss"), Macro("'e"))
  }

  test("Basic LaTeX parse errors") {
    def failParse(src : String) = {
      assert(parser.parseOpt(src) == None, "Parsing should fail on the input \"\"\"" + src + "\"\"\"")
    }

    failParse("""$x""")
    failParse("""{Hello""")
    failParse("""}World""")
    failParse("""${x+y$}""")
    failParse("""{$x+y}$""")
  }

  test("LaTeX rendering") {
    def checkRender(src : String, goal : String) = assert(parser.renderLaTeX(parser.parse(src)) === goal)

    checkRender("""$\mathbb{B}^v \in \text{the set}$""", """𝔹^v ∈ the set""")
    checkRender("""Vercing{\'{e}}torix""", """Vercingétorix""")
    checkRender("""$\beta$-reduced""", "β-reduced")
  }
}
