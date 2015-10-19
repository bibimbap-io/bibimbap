package io.bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import scala.io.Source
import java.io.{FileWriter, File}

class Consolidate(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "consolidate"

  override val dependsOn = Set("search", "results")

  lazy val searchModule  = modules("search")
  lazy val resultsModule = modules("results")

  private var origSender      = sender
  private var consolidatePath = ""
  private var entries         = Stream[BibTeXEntry]()
  private var entriesMap      = Map[BibTeXEntry, Option[BibTeXEntry]]()

  def postfixPath(path: String, postfix: String): String = {
    val newPath = path.replaceAll("\\.bib$", postfix+".bib")

    if (newPath == path) {
      path+postfix
    } else {
      newPath
    }
  }


  def normal: Receive = {
    case Command2("merge", path) =>
      val origSender = sender
      // First we load entries from path
      searchAllOf(path) { (modified, entries) =>
        console ! Success("Found "+entries.size+" entries ("+modified+" modified):")

        syncCommand(resultsModule, SearchResults(entries.map { e =>
          SearchResult(e, Set(), 1)
        }))

        syncCommand(resultsModule, ShowResults(Nil))

        origSender ! CommandSuccess
      }

    case Command2("consolidate", path) =>
      val origSender = sender

      // First we load entries from path
      val consolidatePath = postfixPath(path, "-consolidated")
      searchAllOf(path) { (modified, entries) =>

        try {
          val fw = new FileWriter(new File(consolidatePath), false)

          for (entry <- entries) {
            fw.write(entry.toString)
            fw.write("\n\n")
          }

          fw.close

          console ! Success("Modified "+modified+" entries.")
          console ! Success("Consolidated file saved to "+consolidatePath)

          origSender ! CommandSuccess
        } catch {
          case e: Throwable =>
            origSender ! CommandException(e)
        }

        origSender ! CommandSuccess
      }

    case Command2("lint", path) =>
      try {
        val parser      = new BibTeXParser(Source.fromFile(path), console ! Warning(_))
        val newPath     = postfixPath(path, "-lint")

        val fw = new FileWriter(new File(newPath), false)

        for (entry <- parser.entries) {
          fw.write(entry.toString)
          fw.write("\n\n")
        }

        fw.close

        console ! Success("Reformatted file saved to "+newPath)

        sender ! CommandSuccess
      } catch {
        case e: Throwable =>
          sender ! CommandException(e)
      }

    case x =>
      super.receive(x)
  }

  def searchAllOf(path: String)(onEnd: (Int, List[BibTeXEntry]) => Unit) = {
    try {
      val parser      = new BibTeXParser(Source.fromFile(path), console ! Warning(_))
      entries         = parser.entries
      entriesMap      = Map()
      origSender      = sender

      if (entries.size > 0) {
        context.become(processing(onEnd))

        for (entry <- entries) {
          searchModule ! SearchSimilar(entry)
        }
      } else {
        context.become(normal)
        onEnd(0, Nil)
      }

    } catch {
      case e: Throwable =>
        sender ! CommandException(e)
        context.become(normal)
    }
  }

  def processing(onEnd: (Int, List[BibTeXEntry]) => Unit): Receive = {
    case SimilarEntry(oldEntry, optNewEntry) =>

      entriesMap += oldEntry -> optNewEntry

      if (entries.size > 100 && (entriesMap.size % 40 == 0)) {
        val progress = (entriesMap.size*100d)/entries.size
        console ! Out("   "+("%3d".format(progress.toInt))+"% ("+entriesMap.size+"/"+entries.size+")")
      }

      if (entriesMap.size == entries.size) {

        // We have all the results now!
        var modified = 0

        val results = for (entry <- entries) yield {
          val entr = entriesMap(entry) match {
            case Some(newEntry) if newEntry like entry =>
              var fields = entry.entryMap

              for ((k, v) <- newEntry.entryMap if !fields.contains(k)) {
                fields += k -> v
              }
              BibTeXEntry.fromEntryMap(entry.tpe, entry.key, fields, console ! Error(_)).getOrElse(entry)
            case _ =>
              entry
          }

          if (entr != entry) {
            modified += 1
          }

          entr
        }

        context.become(normal)
        onEnd(modified, results.toList)
      }
  }

  override def receive: Receive = normal

  override def complete(buffer: String, pos: Int): (List[String], Int) = {
    val Lint        = FileCompletor("lint ")
    val Consolidate = FileCompletor("consolidate ")

    (buffer, pos) match {
      case Lint(alts, pos) =>
        (alts, pos)
      case Consolidate(alts, pos) =>
        (alts, pos)
      case _ =>
        (Nil, 0)
    }
  }

  val helpItems = Map(
    "consolidate" -> HelpEntry("consolidate <path>", "Iteratively consolidate the entries found in <path>."),
    "lint"        -> HelpEntry("lint <path>",        "Parse and pretty-print <path>.")
  )
}
