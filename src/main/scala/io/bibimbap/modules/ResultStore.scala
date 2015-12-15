package io.bibimbap
package modules

import akka.actor._
import bibtex._

class ResultStore(val ctx: Context) extends Module {
  val name = "results"

  private var results = List[SearchResult]()

  override def receive: Receive = {
    case Command1("list") | Command1("show") | Command1("last") =>
      displayResults(Nil)
      sender ! CommandProcessed

    case Command2("bib", Indices(ids)) =>
      ids.within(results) match {
        case Some(rs) =>
          for (r <- rs) {
            doBib(r)
          }
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandProcessed

    case Command2("open", Indices(ids)) =>
      ids.within(results) match {
        case Some(rs) =>
          for (r <- rs) {
            doOpen(r)
          }
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandProcessed

    case Command2("show", Indices(ids)) =>
      ids.within(results) match {
        case Some(rs) =>
          for (r <- rs) {
            doShow(r)
          }
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandProcessed

    case SearchResults(newResults) =>
      results = newResults

      sender ! CommandProcessed

    case ReplaceResults(ids, newResults) =>
      ids.within(results) match {
        case Some(rs) =>
          results = results.map((rs zip newResults).toMap.orElse{ case x => x })
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandProcessed

    case GetResults(ids) =>
      sender ! SearchResults(ids.within(results).getOrElse(Nil))

    case ShowResults(terms) =>
      displayResults(terms)
      sender ! CommandProcessed

    case x =>
      super.receive(x)
  }

  private def doOpen(res: SearchResult) {
    import java.awt.Desktop
    import java.net.URI

    val ourl = res.entry.getURL

    ourl match {
      case Some(url) =>
        try {
          Desktop.getDesktop.browse(new URI(url))
        } catch {
          case ue: java.lang.UnsupportedOperationException =>
            import scala.sys.process._

            val logger = new ProcessLogger {
              def out(s : =>String) = {}
              def err(s : =>String) = console ! Error(s)
              def buffer[T](f : =>T) = f
            }

            // Trying an alternative using url.open
            settings.get("url", "open") match {
              case Some(cmd) =>
                (cmd+" "+url).run(logger)
              case None =>
                console ! Error("Error opening "+res.entry.getKey+": Desktop is unsupported and no url.open setting was found")
            }
          case e: Throwable =>
            console ! Error("Error opening "+res.entry.getKey+": "+e.getMessage.trim)
        } 
      case _ =>
        console ! Error("Error opening "+res.entry.getKey+": No url found")
    }
  }

  private def doShow(res: SearchResult) {
    def inBold(str: String): String    = settings.BOLD+str+settings.RESET
    def inRedBold(str: String): String = settings.BOLD+settings.RED+str+settings.RESET

    console ! Out("  Source(s)  : "+res.sources.mkString(", "))
    console ! Out("")

    res.entry.display(console ! Out(_), inBold, inRedBold)
  }

  private def doBib(res: SearchResult) {
    console ! Out(res.entry.toString)
  }

  case class ResultFlag(has: SearchResult => Boolean, symbol: String, legend: String)

  val flagsColumns = List(
    List(
      ResultFlag(_.isEdited, settings.YELLOW+settings.BOLD+"e"+settings.RESET, "Edited")
    ),
    List(
      ResultFlag(_.alternatives.size > 1, settings.BLUE+settings.BOLD+"+"+settings.RESET, "Multiple Alternatives")
    ),
    List(
      ResultFlag({res => res.isManaged && res.entry.isValid  },  settings.GREEN+"\u2714"+settings.RESET,   "Managed"),
      ResultFlag({res => res.isManaged && !res.entry.isValid },  settings.RED+"\u2714"+settings.RESET,     "Managed (incomplete)"),
      ResultFlag({res => !res.isManaged && !res.entry.isValid }, settings.RED+"\u2049"+settings.RESET,     "Incomplete")
    )
  )

  private def displayResults(terms: List[String]) {
    def highlight(str: String): String = {
      if (!terms.isEmpty) {
        import java.util.regex.Pattern
        str.replaceAll("(?i)"+terms.map(Pattern.quote).mkString("(", "|", ")"), settings.UNDERLINED+"$0"+settings.RESET)
      } else {
        str
      }
    }

    var columnsUsed = Set[Int]()
    var flagsUsed   = Set[ResultFlag]()

    // Pre-checks what columns will be displayed
    for (res <- results; (flags, column) <- flagsColumns.zipWithIndex; flag <- flags if flag.has(res)) {
      columnsUsed += column
      flagsUsed += flag
    }

    for ((res, i) <- results.zipWithIndex) {
      val spc = if ((i < 10) && (results.size > 10)) " " else ""

      val columns = for (col <- columnsUsed.toSeq.sorted) yield {
        flagsColumns(col).find(_.has(res)) match {
          case Some(flag) =>
            flag.symbol
          case None =>
            " "
        }
      }

      console ! Out(" "+columns.mkString+" "+spc+"["+i+"] "+highlight(res.entry.inlineString))
    }

    if (!flagsUsed.isEmpty) {
      console ! Out("")
      console ! Out(" Legend:")
      for (flag <- flagsUsed) {
        console ! Out("   "+flag.symbol+" : "+flag.legend)
      }
    }
    if (results.isEmpty) {
      console ! Info("No result")
    }
  }

  val helpItems = Map(
    "list"   -> HelpEntry("list",             "Displays the current list of results."),
    "last"   -> HelpEntry("last",             "Displays the current list of results."),
    "bib"    -> HelpEntry("bib  <result>",    "Displays the bib entry for the <results>th search result."),
    "show"   -> HelpEntry("show <result>",    "Displays the entry for the <results>th search result."),
    "open"   -> HelpEntry("open <result>",    "Opens the corresponding entry's url.")
  )
}
