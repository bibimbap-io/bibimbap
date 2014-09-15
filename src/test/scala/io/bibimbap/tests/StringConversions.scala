package io.bibimbap.tests

import io.bibimbap.strings._

import org.scalatest.FunSuite
import org.scalatest.Matchers

class StringConversions extends FunSuite with Matchers {
  test("Transliteration") {
    def produces(in : String, out : String) {
      MString.fromJava(in).toASCII should equal (out)
    }

    produces("Keyser Söze", "Keyser Soeze")
    produces("Lettøl from Ålborg", "Lettoel from AAlborg")
    produces("β-conversion", "beta-conversion")
    produces("élémentaire", "elementaire")
    produces("Warning ⚠ ", "Warning")
  }

  test("To LaTeX") {
    def produces(in : String, out : String) {
      MString.fromJava(in).toLaTeX should equal (out)
    }

    produces("Keyser Söze", """Keyser S\"{o}ze""")
    produces("Lettøl from Ålborg", """Lett{\o}l from {\AA}lborg""")
    produces("β-conversion", """$\beta$-conversion""")
    produces("élémentaire", """\'{e}l\'{e}mentaire""")
    produces("C#", """C{\#}""")
  }

  test("fromLaTeX/toLaTeX is idempotent") {
    def c(str : String) {
      MString.fromLaTeX(str).toLaTeX should equal (str)
    }

    c("""No LaTeX""")
    c("""{J}ava""")
  }

  test("Conjoining") {
    val j1 = MString.fromJava("salt")
    val j2 = MString.fromJava("pepper")
    assert(MString.conjoin(Nil).toJava === "")
    assert(MString.conjoin(List(j1)).toJava === "salt")
    assert(MString.conjoin(List(j1,j2)).toJava === "salt and pepper")
  }
}
