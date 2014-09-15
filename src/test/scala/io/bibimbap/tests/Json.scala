package io.bibimbap.tests

import io.bibimbap.json._

import org.scalatest.FunSuite
import org.scalatest.Matchers

class JsonParsing extends FunSuite with Matchers {
  private def parsesAs(in : String, v : JValue) {
    (new JSONParser).parse(in) should equal (v)
  }

  test("Base cases") {
    parsesAs("""null""", JNull)

    parsesAs(""" true""", JTrue)

    parsesAs("""false """, JFalse)

    parsesAs("""42""", JInt(42))

    parsesAs(""" "xyz" """, JString("""xyz"""))

    parsesAs(""" "xy\"z" """, JString("""xy"z"""))

    parsesAs("""[ 1, 2, 3 ]""", JArray(JInt(1) :: JInt(2) :: JInt(3) :: Nil))

    parsesAs("""{
      "foo" : 42,
      "bar" : "bar"
    }""", JObject(Map("foo" -> JInt(42), "bar" -> JString("bar"))))
  }

  test("Selectors") {
    val needle = JString("needle")
    val stack0 = JObject(Map("bar" -> needle))
    val stack1 = JArray(JInt(4) :: JInt(2) :: Nil)
    val haystack = JObject(Map("fuu" -> stack1, "foo" -> stack0))

    (haystack \ "foo" \ "bar") should equal (needle)
    (haystack \ "fuu" \ "foo" \ "bar") should equal (JNull)
  }
}
