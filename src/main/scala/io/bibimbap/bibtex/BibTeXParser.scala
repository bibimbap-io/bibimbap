package io.bibimbap
package bibtex

import bibtex._
import strings._
import util.StringUtils

import scala.io.Source

import scala.collection.mutable.{Map=>MutableMap}

class BibTeXParser(src : Source, error : String=>Unit) {
  case class BibTeXParseError(msg : String) extends Exception(msg)
  private val lexer = new Lexer
  private case class RawEntry(kind : String, key : String, pairs : Map[String,String])

  private val constantMap : MutableMap[String,String] = MutableMap(
    "jan" -> "January",
    "feb" -> "February",
    "mar" -> "March",
    "apr" -> "April",
    "may" -> "May",
    "jun" -> "June",
    "jul" -> "July",
    "aug" -> "August",
    "sep" -> "September",
    "oct" -> "October",
    "nov" -> "November",
    "dec" -> "December"
  )

  private def posString(position : Int) : String = {
    "(" + Position.line(position) + ":" + Position.column(position) + ")"
  }

  def entries : Stream[BibTeXEntry] = rawEntries.flatten.flatMap { raw =>
    val newMap : Map[String,MString] = raw.pairs.mapValues(s => MString.fromLaTeX(s))
    val kind = BibTeXEntryTypes.withNameOpt(raw.kind)
    BibTeXEntry.fromEntryMap(kind, Some(raw.key), newMap, error)
  }

  private var definedXRef = Map[String, RawEntry]()
  private var pendingXRef = Map[String, Set[RawEntry]]().withDefaultValue(Set())

  private var firstTime : Boolean = true

  private def rawEntries : Stream[Option[RawEntry]] = {
    if(firstTime) {
      firstTime = false
      nextToken
    }

    if(lastToken == EOF()) {
      if (!pendingXRef.isEmpty) {
        error("Found entries linking to missing crossrefs: "+pendingXRef.keySet.mkString(", "))
      }
      Stream.empty ++ pendingXRef.values.flatten.map(Some(_))
    } else {
      val e = parseEntry

      val prefix = e match {
        case Some(xr) =>
          val res = pendingXRef(xr.key).map(pe => Some(inlineXRef(pe, xr)))
          pendingXRef -= xr.key
          Stream.empty ++ res
        case None =>
          Stream.empty
      }

      prefix append Stream.cons(e, rawEntries)
    }
  }

  private def inlineXRef(to: RawEntry, xref: RawEntry): RawEntry = {
    var newPairs = to.pairs - "crossref"

    var fields = BibTeXEntryTypes.relevantFieldsFor(BibTeXEntryTypes.withNameOpt(to.kind))

    for (field <- fields if !newPairs.contains(field) && xref.pairs.contains(field)) {
      newPairs += field -> xref.pairs(field)
    }

    to.copy(pairs = newPairs)
  }

  private def parseError(msg : String, token : Token) : Nothing = throw new BibTeXParseError(msg + " " + posString(token.position))
  private def tryOrSkipUntil[A](block : =>A)(eatUntil : Token=>Boolean) : Option[A] = {
    try {
      Some(block)
    } catch {
      case pe : BibTeXParseError => {
        error(pe.getMessage)
        while(!eatUntil(lastToken) && lastToken != EOF()) {
          nextToken
        }
        None
      }
    }
  }

  private var lastToken : Token = null
  private def nextToken : Token = {
    lastToken = lexer.nextToken
    lastToken match {
      case e @ ERROR(msg) => parseError(msg, e)
      case good => good
    }
  }

  private def eat(tester : Token=>Boolean) : Unit = {
    if(tester(lastToken)) {
      nextToken
    } else {
      parseError("Unexpected token : " + lastToken, lastToken)
    }
  }

  private def parseEntry : Option[RawEntry] = {
    val kind = tryOrSkipUntil {
      eat(_ == AT())
      parseID.toLowerCase
    }(_ == AT())

    val result = kind.flatMap { _ match {
      case "preamble" => while(lastToken != AT()) { nextToken }; None
      case "comment"  => while(lastToken != AT()) { nextToken }; None

      case "string"   => {
        tryOrSkipUntil {
          eat(_ == BLOCKSTART())
          val key = parseID
          eat(_ == EQUALS())
          val value = parseString
          eat(_ == BLOCKEND())
          constantMap(key.toLowerCase) = value
        }(_ == AT())
        None
      }

      case otherKind => tryOrSkipUntil {      
        eat(_ == BLOCKSTART())
        val key = parseID
        eat(_ == COMMA())

        var pairs : List[(String,String)] = Nil

        while(lastToken != BLOCKEND() && lastToken != EOF()) {
          val pairOpt = tryOrSkipUntil(parsePair)(t => t == COMMA() || t == BLOCKEND())
          pairOpt.foreach(pair => pairs = pair :: pairs)

          while(lastToken == COMMA()) {
            nextToken
          }
        }
        eat(_ == BLOCKEND())
        new RawEntry(otherKind, key, pairs.toMap)
      }(_ == AT())
    }}

    // check whether Xref is defined
    result match {
      case Some(e) =>
        definedXRef += e.key -> e

        e.pairs.get("crossref") match {
          case Some(xr) if definedXRef contains xr =>
            Some(inlineXRef(e, definedXRef(xr)))

          case Some(xr) =>
            pendingXRef += xr -> (pendingXRef(xr) + e)
            None

          case None =>
            Some(e)
        }
      case None =>
        None
    }
  }

  private def parseID : String = lastToken match {
    case ID(value) => nextToken; value
    case _ => parseError("Expected : identifier", lastToken)
  }

  private def parseString : String = {
    val s1 = parseSingleString

    val builder = new StringBuilder
    builder.append(s1)

    while(lastToken == SHARP()) {
      nextToken
      val s2 = parseSingleString
      builder.append(s2)
    }

    StringUtils.normalizeSpace(builder.toString)
  }

  private def parseSingleString : String = lastToken match {
    case STRING(value) => nextToken; value
    case NUM(value) => nextToken; value.toString
    case ID(value) => {
      val subst = constantMap.get(value.toLowerCase) match {
        case Some(v) => v
        case None => error("Constant string " + value + " is undefined. " + posString(lastToken.position)); ""
      }
      nextToken
      subst 
    }
    case _ => parseError("Expected : value, found : " + lastToken, lastToken)
  }


  private def parsePair : (String,String) = {
    val key = parseID.toLowerCase
    eat(_ == EQUALS())
    val value = parseString
    (key -> value)
  }

  // Below is point is where the lexer sits.

  private sealed abstract class Token(rep : String) {
    private var pos : Int = -1
    def setPos(p : Int) : this.type = {
      pos = p
      this
    }
    def position : Int = pos
    override def toString : String = rep
  }
  private case class EOF() extends Token("EOF")
  private case class AT() extends Token("@")
  private case class BLOCKSTART() extends Token("{")
  private case class BLOCKEND() extends Token("}")
  private case class COMMA() extends Token(",")
  private case class EQUALS() extends Token("=")
  private case class SHARP() extends Token("#")
  private case class ID(value : String) extends Token("identifier[" + value + "]")
  private case class STRING(value : String) extends Token("value")
  private case class NUM(value : Int) extends Token("integer")
  private case class ERROR(reason : String) extends Token("Error: " + reason)

  private class Lexer {
    private var lastChar : Char = _
    private var previousChar : Char = _
    private var tokenPos : Int = -1
    private var currentToken : Token = _
    private val builder = new StringBuilder
    private var inEntry : Boolean = false
    private var inEntryBlock : Boolean = false
    private val EndOfFile : Char = Char.MaxValue

    private def nextChar : Char = {
      def readChar: Char = if(src.hasNext) {
        src.next
      } else {
        EndOfFile
      }

      if(lastChar == EndOfFile) EndOfFile else {
        lastChar = readChar
        previousChar = if ((previousChar == '\r') && (lastChar == '\n')) readChar else lastChar
        lastChar = if (previousChar == '\r') '\n' else previousChar
        lastChar
      }
    }


    private var firstCall : Boolean = true
    def nextToken : Token = {
      if(firstCall) {
        nextChar
        firstCall = false
      }

      while(!inEntry && lastChar != '@' && lastChar != EndOfFile) {
        nextChar
      }
      while(isWhitespace(lastChar)) {
        nextChar
      }

      readToken
    }

    private def isWhitespace(char : Char) : Boolean = Character.isWhitespace(char)
    private def isValidIDFirst(char : Char) : Boolean = Character.isLetter(char)
    private def isValidIDNonFirst(char : Char) : Boolean = char match {
      case _ if Character.isLetterOrDigit(char) => true
      // these should really not be allowed
      case '=' | '{' | '}' | '"' | '@' | '\\' | '#' | '~' | '%' | ',' => false 
      case ':' | '/' | '-' | '_' | '.' => true
      case _ => false
    }
    private def isValidDigit(char : Char) : Boolean = Character.isDigit(char)

    private def readToken : Token = {
      tokenPos = src.pos

      lastChar match {
        case EndOfFile => EOF().setPos(tokenPos)
        case '@' => {
          inEntry = true
          nextChar
          AT().setPos(tokenPos)
        }
        case '{' if !inEntryBlock => {
          inEntryBlock = true
          nextChar
          BLOCKSTART().setPos(tokenPos)
        }
        case '}' if inEntryBlock => {
          inEntryBlock = false
          inEntry = false
          nextChar
          BLOCKEND().setPos(tokenPos)
        }
        case '=' => {
          nextChar
          EQUALS().setPos(tokenPos)
        }
        case ',' => {
          nextChar
          COMMA().setPos(tokenPos)
        }
        case '#' => {
          nextChar
          SHARP().setPos(tokenPos)
        }
        // String blocks
        case ('{' | '"') if inEntryBlock => {
          val matching : Char = lastChar match {
            case '{' => '}'
            case '"' => '"'
          }

          var errorMsg : Option[String] = None

          // to track opening braces
          var openCount : Int = 0

          builder.setLength(0)
          nextChar

          while(!(openCount == 0 && lastChar == matching) && errorMsg.isEmpty) {
            if(lastChar == EndOfFile) {
              errorMsg = Some("Unterminated string.")
            } else {
              if(lastChar == '{') {
                openCount += 1
              } else if(lastChar == '}') {
                openCount -= 1
                if(openCount < 0) {
                  errorMsg = Some("Unmatched } in string.")
                }
              }
              if(lastChar == '\n') {
                lastChar = ' '
              }
              builder.append(lastChar)
              nextChar
            }
          }
          nextChar
          (errorMsg.map(e => ERROR(e)).getOrElse {
            if(openCount > 0) {
              ERROR("Unclosed { in string.").setPos(tokenPos)
            } else {
              STRING(builder.toString).setPos(tokenPos)
            }
          }).setPos(tokenPos)
        }

        case _ if isValidIDFirst(lastChar) => {
          builder.setLength(0)
          do {
            builder.append(lastChar)
          } while(isValidIDNonFirst(nextChar)) // side-effecting tests forever <3
          ID(builder.toString).setPos(tokenPos)
        }

        case _ if isValidDigit(lastChar) => {
          builder.setLength(0)
          do {
            builder.append(lastChar)
          } while(isValidDigit(nextChar)) 
          val value = BigInt(builder.toString).toInt
          NUM(value).setPos(tokenPos)
        }
      }
    }
  }

  object Position {
    /** Number of bits used to encode the line number */
    final val LINE_BITS   = 20
    /** Number of bits used to encode the column number */
    final val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
    /** Mask to decode the line number */
    final val LINE_MASK   = (1 << LINE_BITS) - 1
    /** Mask to decode the column number */
    final val COLUMN_MASK = (1 << COLUMN_BITS) - 1 

    def line(pos: Int): Int = (pos >> COLUMN_BITS) & LINE_MASK
    def column(pos: Int): Int = pos & COLUMN_MASK
  }
}
