package io.bibimbap
package modules

import akka.actor._
import scala.concurrent.Future
import akka.pattern.{ask, pipe}
import java.util.concurrent.TimeoutException

import org.jsoup._
import org.jsoup.nodes._

import bibtex._


class Abstract(val ctx: Context) extends Module {

  val name = "abstract"

  override def receive: Receive = {
    case Command2("abstract", Indices(ids)) =>
      (for {
        results <- (modules("results") ? GetResults(ids)).mapTo[SearchResults]
        abss <- getAbstracts(results.entries)
      } yield {

        for ((res, oabs) <- abss) {
          oabs match {
            case Some(abs) =>

              val limit = 80

              console ! Out(res.entry.inlineString)
              console ! Out("-"*limit)
              val lines = abs.split('\n').toList

              val formatted = lines.flatMap { l =>
                l.split(" ").toList.foldLeft(List[String]()) {
                  (l, w) =>
                    if (l.isEmpty || l.head.length + w.length > limit) {
                      w :: l
                    } else {
                      (l.head + " " + w) :: l.tail
                    }
                }.reverse
              }.mkString("\n")

              console ! Out(formatted)
              console ! Out("-"*limit)

            case None =>
              console ! Warning("Abstract for '"+res.entry.inlineString+"' unavailable")
          }
        }

        CommandProcessed
      }) pipeTo sender


    case x =>
      super.receive(x)
  }

  private def getAbstracts(results: List[SearchResult]): Future[List[(SearchResult, Option[String])]] = {
    Future.sequence(results.map(getAbstract)).map {
      res => results.zip(res)
    }
  }

  private def getAbstract(res: SearchResult): Future[Option[String]] = {
    res.entry.getURL match {
      case Some(url) =>
        val request = wsClient.url(url).withRequestTimeout(5000)

        (for (res <- request.get()) yield {
          val body = res.body

          val doc = Jsoup.parse(body)

          matchSpringer(doc) orElse
          matchArxiv(doc) orElse
          matchACM(doc)

        }).recover {
          case ex =>
            console ! Warning("Unnexpected error while fetching abstract: "+ex.getMessage)
            None
        }
      case None =>
        Future(None)

    }
  }

  private def matchArxiv(doc: Document): Option[String] = {
    val abs = doc.select("""blockquote.abstract""")

    if (abs.isEmpty) {
      None
    } else {
      Some(abs.text())
    }
  }

  private def matchSpringer(doc: Document): Option[String] = {
    val abs = doc.select("""section[class="Abstract"] p""")

    if (abs.isEmpty) {
      None
    } else {
      Some(abs.text())
    }
  }

  private def matchACM(doc: Document): Option[String] = {
    val abs = doc.select("""#abstract""")

    if (abs.isEmpty) {
      None
    } else {
      Some(abs.text())
    }
  }

  val helpItems = Map(
    "abstract"        -> HelpEntry("abstract <index>",         "Obtain abstract for search results <index>")
  )
}
