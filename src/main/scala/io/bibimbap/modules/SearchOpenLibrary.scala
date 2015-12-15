package io.bibimbap
package modules

import akka.actor._
import scala.concurrent.Future
import java.util.concurrent.TimeoutException

import bibtex._
import strings._
import identifiers.ISBN
import play.api.libs.json._
import scala.io.Source

/** Currently, this provider can only lookup ISBN numbers. */
class SearchOpenLibrary(val ctx: Context) extends SearchProvider {
  val name   = "Search Open Library"
  val source = "openlibrary"

  import ctx._

  private val apiURL = "http://openlibrary.org/api/books"

  override def search(terms : List[String], limit : Int): Future[List[SearchResult]] = {
    ISBN.extract(terms.mkString("")) match {
      case Some(isbn) =>

        val isbnKey = "ISBN:"+isbn

        val request = wsClient.url(apiURL).withQueryString(
          "bibkeys" -> isbnKey,
          "jscmd" -> "data",
          "format" -> "json"
        ).withHeaders("Accept" -> "application/json")
        .withRequestTimeout(2000)

        (for (response <- request.get()) yield {
           response.status match {
              case 200 =>
                (response.json \ isbnKey).asOpt[JsValue] match {
                  case Some(hit) =>
                    openLibToSearchResult(hit).toList
                  case _ =>
                    console ! Warning("Unexpected json output from OpenLibrary API!")
                    Nil
                }


              case code =>
                console ! Warning("Request to Open Library failed with code: "+code)
                Nil
           }
        }).recover {
          case to: TimeoutException =>
            console ! Warning("Timeout reached while querying OpenLibrary! Please try another time")
            Nil

          case ex =>
            console ! Error("Unnexpected Error: "+ex.getMessage)
            Nil
        }

      case None =>
        Future(Nil)
    }
  }

  private def openLibToSearchResult(jvalue: JsValue): Option[SearchResult] = {
    val title = (jvalue \ "title").asOpt[String]

    val authors = (jvalue \ "authors").asOpt[Seq[JsValue]] match {
      case Some(elems) =>
        elems.flatMap(e => (e \ "name").asOpt[String])
      case None =>
        Seq()
    }

    val authorsConcat = if(authors.isEmpty) {
      None
    } else {
      Some(authors.mkString(" and "))
    }

    val publisher = (jvalue \ "publishers").asOpt[Seq[JsValue]] match {
      case Some(elems) =>
        elems.flatMap(e => (e \ "name").asOpt[String]).headOption
      case None =>
        None
    }

    val year = (jvalue \ "publish_date").asOpt[String]

    val address = (jvalue \ "publish_places").asOpt[Seq[JsValue]] match {
      case Some(elems) =>
        elems.flatMap(e => (e \ "name").asOpt[String]).headOption
      case None =>
        None
    }

    // TODO: keep ISBN around, look for potential editors, month, etc.

    val emap : Map[String, String] = Map[String,Option[String]](
      "title" -> title,
      "author" -> authorsConcat,
      "publisher" -> publisher,
      "address" -> address,
      "year" -> year
    ).filterNot(_._2.isEmpty).mapValues(_.get)

    val entry = BibTeXEntry.fromEntryMap(
      Some(BibTeXEntryTypes.Book), None, emap.mapValues(MString.fromJava),
      console ! Error(_))

    entry.map(SearchResult(_, Set(source), 1.0))
  }
}
