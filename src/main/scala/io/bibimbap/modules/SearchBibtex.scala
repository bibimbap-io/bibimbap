package io.bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import bibtex._

import scala.io.Source

class SearchBibtex(val ctx: Context, val path: String) extends LuceneRAMBackend with LuceneSearchProvider {
  val name = "SearchBibtex"

  val source = "bibtex - "+path

  import ctx._

  override def receive: Receive = {
    case InitializeSource =>
      try {
        val parser = new BibTeXParser(Source.fromFile(path), console ! Warning(_))
        addEntries(parser.entries)

        sender ! true
      } catch {
        case e: Throwable =>
          console ! Error(e.getMessage)
          sender ! false
      }
    case x =>
      super.receive(x)
  }
}
