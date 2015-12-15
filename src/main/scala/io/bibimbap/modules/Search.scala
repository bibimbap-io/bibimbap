package io.bibimbap
package modules

import akka.actor._
import scala.concurrent.Future
import akka.pattern.{ask, pipe}
import java.util.concurrent.TimeoutException

import bibtex._


class Search(val ctx: Context) extends Module {

  case class SearchSource(actor: ActorRef, name: String, var isActive: Boolean = true)

  val name = "search"

  var searchSources = List[SearchSource]();

  lazy val resultsModule = modules("results")

  override def postInitialization() = {
    searchSources = List(
      SearchSource(modules("managed"), "managed file"),
      SearchSource(context.actorOf(Props(classOf[SearchLocal], ctx),       name = "searchLocal"), "local cache"),
      SearchSource(context.actorOf(Props(classOf[SearchDBLP], ctx),        name = "searchDBLP"), "DBLP"),
      SearchSource(context.actorOf(Props(classOf[SearchOpenLibrary], ctx), name = "searchOpenLibrary"), "Open Library")
    )
  }

  private def addSource(path: String): Future[CommandResult] = {
    val actor = context.actorOf(Props(classOf[SearchBibtex], ctx, path))

    for {
      r <- (actor ? InitializeSource).mapTo[Boolean]
    } yield {
      if (r) {
        searchSources = searchSources :+ SearchSource(actor, "bibtex: "+path)
        console ! Success("Source '"+path+"' loaded")
      } else {
        console ! Error("Failed to load source '"+path+"'")
      }

      CommandProcessed
    }
  }

  override def receive: Receive = {
    case Command1("sources") =>
      for ((s, i) <- searchSources.zipWithIndex) {
        val spc = if ((i < 10) && (searchSources.size > 10)) " " else ""

        val status = if(s.isActive) {
          settings.GREEN+"on "+settings.RESET
        } else {
          settings.RED+"off"+settings.RESET
        }

        console ! Out(" "+status+" "+spc+"["+i+"] "+s.name)
      }
      sender ! CommandProcessed

    case Command3("sources", "enable", Indices(ids)) =>
      ids.within(searchSources) match {
        case Some(rs) =>
          for (r <- rs) {
            r.isActive = true
          }
          console ! Success("Source(s) activated")
        case None =>
          console ! Error("Invalid source")
      }
      sender ! CommandProcessed

    case Command3("sources", "disable", Indices(ids)) =>
      ids.within(searchSources) match {
        case Some(rs) =>
          for (r <- rs) {
            r.isActive = false
          }
          console ! Success("Source(s) disabled")
        case None =>
          console ! Error("Invalid source")
      }
      sender ! CommandProcessed

    case Command3("sources", "add", path) =>
      addSource(path) pipeTo sender

    case Command2("load", path) =>
      addSource(path) pipeTo sender

    case CommandL("search", args) =>

      val r = for {
        results <- doSearch(args, 10)
        r1 <- resultsModule ? SearchResults(results)
        r2 <- resultsModule ? ShowResults(args)
      } yield { CommandProcessed }

      r pipeTo sender

    case ImportedResult(res) =>
      for (m <- searchSources.map(_.actor)) {
        m ! ImportedResult(res)
      }

    case Search(terms, limit) =>
      doSearch(terms, limit) pipeTo sender

    case SearchSimilar(entry: BibTeXEntry) =>
      if (preciseEnough(entry)) {
        val r = for (results <- doSearch(termsFromEntry(entry), 1)) yield {
          SimilarEntry(entry, results.headOption.map(_.entry))
        }

        r pipeTo sender
      } else {
        sender ! SimilarEntry(entry, None)
      }

    case x =>
      super.receive(x)
  }

  private def preciseEnough(e: BibTeXEntry): Boolean = {
    !e.title.isEmpty
  }

  private def termsFromEntry(e: BibTeXEntry): List[String] = {
    (e.title ++ e.authors).map(_.toJava).toList
  }

  def activeSources: List[SearchSource] = searchSources.filter(_.isActive)

  private def doSearch(args: List[String], limit: Int): Future[List[SearchResult]] = {

    val searches = for (ss <- activeSources.map(_.actor)) yield {
      (ss ? Search(args, limit)).mapTo[List[SearchResult]]
    }

    def mergeResults(existing: List[SearchResult], fresh: List[SearchResult]): List[SearchResult] = {
      var equivClasses = Map[SearchResult, List[SearchResult]]() ++ existing.map(e => e -> List(e))

      for (res <- fresh) {
        equivClasses.keySet.find(k => k.entry like res.entry) match {
          case Some(k) =>
            // Found a matching equiv class
            equivClasses += k -> (equivClasses(k) :+ res)

          case None =>
            equivClasses += res -> List(res)
        }
      }

      val results = for ((v, alts) <- equivClasses) yield {
        if (alts.size == 1) {
          Some(v)
        } else {
          val main   = alts.find(_.sources contains "managed").getOrElse(alts.head)
          val others = alts diff List(main)

          val mainEntry = main.entry

          var fields = mainEntry.entryMap

          for (entry <- others.map(_.entry); (k, v) <- entry.entryMap if !fields.contains(k)) {
            fields += k -> v
          }

          val tpe = alts.map(_.entry.tpe).reduceLeft(_ orElse _)

          BibTeXEntry.fromEntryMap(tpe, mainEntry.key, fields, console ! Error(_)) match {
            case Some(newEntry) =>
              if (newEntry != mainEntry) {
                Some(SearchResult(newEntry,
                             alts.flatMap(_.sources).toSet,
                             alts.map(_.relevance).max,
                             isEdited     = alts.exists(_.isEdited),
                             isManaged    = alts.exists(_.isManaged),
                             oldEntry     = alts.map(_.oldEntry).reduceLeft(_ orElse _),
                             alternatives = alts.map(_.entry).toSet
                           ))
              } else {
                // We keep only the managed one if it remains the same
                Some(main)
              }
            case _ =>
              None
          }
        }
      }

      results.flatten.toList.sortBy(r => -r.relevance)
    }

    Future.fold(searches)(List[SearchResult]())(mergeResults)
  }

  private def combineResults(resultss: List[SearchResults]): List[SearchResult]= {
    import scala.collection.mutable.ListBuffer

    var equivClasses = ListBuffer[ListBuffer[SearchResult]]()

    for (res <- resultss.flatMap(_.entries)) {
      equivClasses.find(_.head.entry like res.entry) match {
        case Some(cl) => cl.append(res)
        case None     => equivClasses.append(new ListBuffer[SearchResult]() :+ res)
      }
    }

    val combined = for (res <- equivClasses) yield {
      var mainEntry = res.head.entry
      var fields = mainEntry.entryMap

      for (entry <- res.tail.map(_.entry); (k, v) <- entry.entryMap if !fields.contains(k)) {
        fields += k -> v
      }

      BibTeXEntry.fromEntryMap(res.map(_.entry.tpe).reduceLeft(_ orElse _), mainEntry.key, fields, console ! Error(_)) match {
        case Some(newEntry) =>
          Some(SearchResult(newEntry,
                       res.flatMap(_.sources).toSet,
                       res.map(_.relevance).max,
                       isEdited     = res.exists(_.isEdited),
                       isManaged    = res.exists(_.isManaged),
                       oldEntry     = res.map(_.oldEntry).reduceLeft(_ orElse _),
                       alternatives = res.map(_.entry).toSet
                     ))
        case _ =>
          None
      }
    }

    val sorted = combined.flatten.toList.sortBy(- _.relevance)

    sorted
  }

  override def complete(buffer: String, pos: Int): (List[String], Int) = {
    val SourcesAdd = FileCompletor("sources add ")
    val Load       = FileCompletor("load ")

    (buffer, pos) match {
      case SourcesAdd(alts, pos) => (alts, pos)
      case Load(alts, pos) => (alts, pos)
      case _ => (Nil, 0)
    }
  }

  val helpItems = Map(
    "search"          -> HelpEntry("search <terms..>",         "Searches for <terms> using the various search providers"),
    "sources"         -> HelpEntry("sources",                  "Lists available search sources"),
    "sources add"     -> HelpEntry("sources add <path>",       "Adds bibfile as additional source"),
    "load"            -> HelpEntry("load <path>",              "Adds bibfile as additional source"),
    "sources enable"  -> HelpEntry("sources enable <index>",   "Enables source"),
    "sources disable" -> HelpEntry("sources disable <index>",  "Disables source")
  )
}
