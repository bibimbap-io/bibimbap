package io.bibimbap.tests.issues

import io.bibimbap.bibtex._
import io.bibimbap.strings._

import org.scalatest.FunSuite
import org.scalatest.Matchers

class GitHub004 extends FunSuite with Matchers {
  test("Issue #4 : name shortening should not crash.") {
    var errorOccurred : Boolean = false

    val entry = BibTeXEntry.fromEntryMap(
      Some(BibTeXEntryTypes.Misc),
      None,
      Map("title" -> MString.fromJava("The Teachings of Confucius"),
          "author" -> MString.fromJava("")),
      (str : String) => { errorOccurred = true }
    )

    errorOccurred should equal (false)

    entry should be ('defined)

    entry.get.inlineString 
  }
}
