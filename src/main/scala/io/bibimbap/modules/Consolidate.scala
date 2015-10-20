package io.bibimbap
package modules

import akka.actor._
import akka.pattern.{ ask, pipe }

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

import bibtex._
import strings._
import scala.io.Source
import java.io.{FileWriter, File}

class Consolidate(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module {
  val name = "consolidate"

  override val dependsOn = Set("search", "results")

  lazy val searchModule  = modules("search")
  lazy val resultsModule = modules("results")

  def postfixPath(path: String, postfix: String): String = {
    val newPath = path.replaceAll("\\.bib$", postfix+".bib")

    if (newPath == path) {
      path+postfix
    } else {
      newPath
    }
  }


  override def receive = {
    case Command2("merge", path) =>
      // First we load entries from path

      val (modified, entries) = Await.result(searchAllOf(path), Duration.Inf)

      console ! Success("Found "+entries.size+" entries ("+modified+" modified):")

      syncCommand(resultsModule, SearchResults(entries.map { e =>
        SearchResult(e, Set(), 1)
      }))

      syncCommand(resultsModule, ShowResults(Nil))

      sender ! CommandSuccess

    case Command2("consolidate", path) =>
      // First we load entries from path
      val (modified, entries) = Await.result(searchAllOf(path), Duration.Inf)

      val consolidatePath = postfixPath(path, "-consolidated")

      try {
        val fw = new FileWriter(new File(consolidatePath), false)

        for (entry <- entries) {
          fw.write(entry.toString)
          fw.write("\n\n")
        }

        fw.close

        console ! Success("Modified "+modified+" entries.")
        console ! Success("Consolidated file saved to "+consolidatePath)

        sender ! CommandSuccess
      } catch {
        case e: Throwable =>
          sender ! CommandException(e)
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

  def searchAllOf(path: String): Future[(Int, List[BibTeXEntry])] = {
    try {
      val parser      = new BibTeXParser(Source.fromFile(path), console ! Warning(_))

      val entries = parser.entries

      val progress = Progress.bounded(console, entries.size)

      val fs = for (entry <- entries) yield {
        (searchModule ? SearchSimilar(entry)).map {
          case SimilarEntry(oldEntry, optNewEntry) =>
            progress.tick
            oldEntry -> optNewEntry
        }
      }

      val emf = Future.fold(fs)(Map[BibTeXEntry, Option[BibTeXEntry]]()) { _ + _ }

      for (entriesMap <- emf) yield {
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

        (modified, results.toList)
      }
    } catch {
      case e: Throwable =>
        Future.failed(e)
    }
  }

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
