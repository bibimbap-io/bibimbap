package bibimbap.tests

import bibimbap.bibtex._

import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BibTeXParsing extends FunSuite with ShouldMatchers {
  def exhaustStream[T](s: Stream[T]): List[T] = s.toList

  def entriesAndErrors(str : String) : (List[BibTeXEntry],Int) = {
    var errorCount : Int = 0
    def errorHandler(s : String) : Unit = { errorCount += 1; }
    val parser = new BibTeXParser(Source.fromString(str), errorHandler)
    val entries = exhaustStream(parser.entries)
    (entries, errorCount)
  }

  test("Two valid entries") {
    val src = """
      This is a dummy BibTeX file.
      
      @inproceedings{Thor2012OnEntries,
        title  = "On " # "BibTeX {"}Entries{"}",
        author = {Alfred U. Thor},
        year   = 2012,
        booktitle = {{BIG}CONF}
      } @article{ ThisKeyIsDroppedByBibimbap ,
        title={All {CAPS}, A Keyboard Memoir},
        author = "Quentin Werty", year=1976, journal="Keystroke Prenvention" }
    """
    
    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (2)
    errors should equal (0)
  }

  test("One valid entry with one broken field") {
    val src = """
      @misc{ThreeGuys1291OnTheMountain,
        author = {Werner Stauffacher and Walter F{\"u}rst and Arnold of Melchtal},
        title  = "R{\"u}tlischwur",
        year   = 2012,
        field  = "Broken String}", 
      } 
    """
    
    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (1)
    errors should equal (1)
  }

  test("Graceful crash on unterminated string.") {
    val src = """
      @misc{SomeEntry,
        author = {John Doe and Jane Doe},
        title  = {On Anonymity},
        annote = "Unterminated string
      }
      
      @Comment{ la la la la la }
    """

    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (0)
    errors should equal (2)
  }

  test("Valid Crossref") {
    val src = """
      @inproceedings{Thor2012OnEntries,
        title  = "On " # "BibTeX {"}Entries{"}",
        author = {Alfred U. Thor},
        year   = 2012,
        crossref = {ThisKeyIsDroppedByBibimbap},
        booktitle = {{BIG}CONF}
      }
      @article{ ThisKeyIsDroppedByBibimbap ,
        title={All {CAPS}, A Keyboard Memoir},
        author = "Quentin Werty", year=1976, journal="Keystroke Prenvention"
      }
    """
    
    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (2)
    errors should equal (0)
  }

  test("Missing Crossref") {
    val src = """
      @inproceedings{Thor2012OnEntries,
        title  = "On " # "BibTeX {"}Entries{"}",
        author = {Alfred U. Thor},
        year   = 2012,
        crossref = {Whatisthis},
        booktitle = {{BIG}CONF}
      }
      @article{ ThisKeyIsDroppedByBibimbap ,
        title={All {CAPS}, A Keyboard Memoir},
        author = "Quentin Werty", year=1976, journal="Keystroke Prenvention"
      }
    """

    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (2)
    errors should equal (1)
  }

  test("Preceeding Crossref") {
    val src = """
      @article{ ThisKeyIsDroppedByBibimbap ,
        title={All {CAPS}, A Keyboard Memoir},
        author = "Quentin Werty", year=1976, journal="Keystroke Prenvention"
      }
      @inproceedings{Thor2012OnEntries,
        title  = "On " # "BibTeX {"}Entries{"}",
        author = {Alfred U. Thor},
        year   = 2012,
        crossref = {ThisKeyIsDroppedByBibimbap},
        booktitle = {{BIG}CONF}
      }
    """
    
    val (entries, errors) = entriesAndErrors(src)
    entries.size should equal (2)
    errors should equal (0)
  }
}
