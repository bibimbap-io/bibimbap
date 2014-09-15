package io.bibimbap.json

import scala.util.parsing.json.Lexer
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import io.bibimbap.{Parser=>BibimbapParser}

class JSONParser extends StdTokenParsers with ImplicitConversions with BibimbapParser[JValue] {
  val typeDesc : String = "JSON"

  // Fill in abstract defs
  type Tokens = Lexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  /** Type signature for functions that can parse numeric literals */
  private type NumericParser = String => Any
 
  // Define the grammar
  private def jsonObj    = "{" ~> repsep(objEntry, ",") <~ "}" ^^ { case vals : List[_] => JObject(Map[String, JValue]() ++ vals) }
  private def objEntry   = stringVal ~ (":" ~> value) ^^ { case x ~ y => (x, y) }

  private def jsonArray  = "[" ~> repsep(value, ",") <~ "]" ^^ { case vals : List[_] => JArray(vals) }

  private def stringVal  = accept("string", { case lexical.StringLit(n) => n} )

  private def dblpMalformedString = "{" ~> stringVal <~ "}"

  private val IsInt = """-?\d+""".r

  private def number     = accept("number", { 
    case lexical.NumericLit(n) => n match {
      case IsInt() => JInt(n.toInt)
      case _       => JDouble(n.toDouble)
  }})

  private def value: Parser[JValue] = (jsonObj |
                               jsonArray |
                               number |
                               "true" ^^^ JTrue |
                               "false" ^^^ JFalse |
                               "null" ^^^ JNull |
                               stringVal ^^ { str => JString(str) } |
                               dblpMalformedString ^^ { str => JString(str) }
                              )

  def parseOpt(content: String) : Either[String,JValue] = {
    phrase(value)(new lexical.Scanner(content)) match {
      case Success(x, _) => Right(x)
      case NoSuccess(err, next) =>
        Left("Failed to parse JSON input (line " + next.pos.line + ", column " + next.pos.column + ").")
    }
  }
}

abstract class JValue {
  def \(k: String): JValue
  def \\(k: String): List[JValue]

  def asOpt[T:JExtractor]: Option[T] = implicitly[JExtractor[T]].extract(this)
}

case class JObject(fields: Map[String, JValue]) extends JValue {
  def \(k: String): JValue = fields.getOrElse(k, JNull)
  def \\(k: String): List[JValue] = fields.get(k).map(List(_)).getOrElse(fields.values.flatMap(_ \\ k).toList)
}

case class JArray(elems: List[JValue]) extends JValue {
  def \(k: String): JValue = elems.headOption.map(_ \ k).getOrElse(JNull)
  def \\(k: String): List[JValue] = elems.flatMap(_ \\ k)
}

abstract class JLit extends JValue {
  def \(k: String): JValue = JNull
  def \\(k: String): List[JValue] = Nil
}

case class JString(v: String) extends JLit
case class JDouble(v: Double) extends JLit
case class JInt(v: Int) extends JLit
case object JTrue  extends JLit
case object JFalse extends JLit
case object JNull  extends JLit

trait JExtractor[T] {
  def extract(j: JValue) : Option[T]
}
