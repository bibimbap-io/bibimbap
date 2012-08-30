package bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import bibtex._
import util.FileUtils

import scala.io.Source
import java.io.{File, FileWriter}
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection

class Managed(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends Module
                                                                                    with LuceneRAMBackend
                                                                                    with LuceneSearchProvider {
  val name = "Managed"

  val source = "managed"

  val managedPath = settings("general", "bib.filename")
  val managedFile = new File(managedPath)
  var managedHash: Option[String] = None

  def computeManagedHash(): Option[String] = FileUtils.md5(managedFile)

  override def searchLucene(query: String) = super.searchLucene(query).map(_.copy(isManaged = true))

  def loadFile() {
    if(managedFile.exists && managedFile.isFile && managedFile.canRead) {

      managedHash  = computeManagedHash()

      val parser = new BibTeXParser(Source.fromFile(managedFile), console ! Error(_))
      addEntries(parser.entries)
    }
  }

  override def receive: Receive = {
    case msg @ OnStartup(os) => {
      super[Module].receive(msg)
      loadFile()
    }

    case Search(terms) =>
      sender ! search(terms)

    case Command2("delete", ind) =>
      syncMessage[SearchResults](modules("results"), GetResults(ind)) match {
        case Some(SearchResults(rs)) =>
          val newResults = rs.map(doDelete)

          syncCommand(modules("results"), ReplaceResults(ind, newResults))
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandSuccess


    case Command2("import", ind) =>
      syncMessage[SearchResults](modules("results"), GetResults(ind)) match {
        case Some(SearchResults(rs)) =>
          val newResults = rs.map(doImport)

          syncCommand(modules("results"), ReplaceResults(ind, newResults))
        case None =>
          console ! Error("Invalid search result")
      }
      sender ! CommandSuccess

    case ImportedResult(res) =>
      // NOOP: we sent this.

    case msg =>
      super[Module].receive(msg)
  }

  private def integrityCheck(): String = {
    var action = "proceed"

    if (managedHash != computeManagedHash()) {
      console ! Warning("Managed file has been modified in the meantime!")
      var ask = true
      while(ask) {
        syncMessage[LineRead](console, ReadLineWithHandle("(p)roceed (c)ancel (r)eload> ")) match {
          case Some(LineRead("c")) =>
            ask = false
            action = "cancel"
          case Some(LineRead("p")) =>
            ask = false
            action = "proceed"
          case Some(LineRead("r")) =>
            ask = false
            action = "reload"
          case Some(LineRead(_)) =>
            console ! Error("wat?")
            ask = true
          case _ =>
            action = "cancel"
            ask = false
        }
      }
    }

    action
  }

  private def writeManagedFile() {
    import org.apache.lucene.index.IndexReader
    val reader = IndexReader.open(index)

    val entries = for (i <- 0 until reader.maxDoc if !reader.isDeleted(i); entry <- documentToEntry(reader.document(i))) yield  entry

    val fw = new FileWriter(managedFile, false)

    for (entry <- entries.sortBy(_.getKey)) {
      fw.write(entry.toString)
      fw.write("\n\n")
    }

    fw.close

    managedHash = computeManagedHash()
  }

  private def doDelete(res: SearchResult): SearchResult = {
    var newRes = res

    if (res.isManaged) {
      val action = integrityCheck()

      if (action == "proceed") {
        deleteEntryByKey(res.entry.getKey)

        writeManagedFile()

        newRes = newRes.copy(isManaged = false)

        console ! Success("Entry no longer managed!")

      } else if(action == "cancel") {
        console ! Success("Aborted")
      } else if (action == "reload") {
        loadFile()
        console ! Success("File reloaded!")
      }
    } else {
      console ! Success("Entry is not in managed, cannot be deleted!")
    }

    newRes
  }

  private def doImport(res: SearchResult): SearchResult = {
    var newRes = res

    def displayImported() {
      try {
        val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
        val stringSel = new StringSelection(res.entry.getKey)
        clipboard.setContents(stringSel, stringSel)
      } catch {
        case e: java.awt.HeadlessException =>
          console ! Warning("Could not store in clipboard: "+e.getMessage.trim)
      }

      console ! Success("Entry key: \\cite{"+res.entry.getKey+"}")
    }

    if (!res.isManaged || res.isEdited) {
      val action = integrityCheck()

      if (action == "proceed") {
        addEntry(res.entry)

        writeManagedFile()

        newRes = newRes.copy(isEdited = false, isManaged = true)

        // Inform search module that we imported this
        modules("search") ! ImportedResult(newRes)

        displayImported()
      } else if(action == "cancel") {
        console ! Success("Aborted")
      } else if (action == "reload") {
        loadFile()
        console ! Success("File reloaded!")
      }
    } else {
      console ! Success("Entry already imported as is!")
      displayImported()
    }

    newRes
  }

  val helpItems = Map(
    "delete" -> HelpEntry("delete <result>",  "Delete the <result>th from the managed file"),
    "import" -> HelpEntry("import <result>",  "Imports the <result>th item from the last search results into managed bib file")
  )
}
