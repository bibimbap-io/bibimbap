package io.bibimbap
package modules

import akka.actor._

import bibtex._
import strings._
import identifiers.ISBN
import play.api.libs.json._
import scalaj.http._
import scala.io.Source

/** Currently, this provider can only lookup ISBN numbers. */
class SearchOpenLibrary(val repl : ActorRef, val console : ActorRef, val settings : Settings) extends SearchProvider {
  val name   = "Search Open Library"
  val source = "openlibrary"

  private val apiURL = "http://openlibrary.org/api/books"

  override def search(terms : List[String], limit : Int) : SearchResults = {
    ISBN.extract(terms.mkString("")) match {
      case Some(isbn) =>

        val isbnKey = "ISBN:"+isbn
        val request = Http(apiURL).param("bibkeys", isbnKey)
                                  .param("format", "json")
                                  .param("jscmd", "data")

        val response = request.timeout(connTimeoutMs = 1000, readTimeoutMs = 2000)
                              .execute(parser = { is => Json.parse(is) })

        response.code match {
          case 200 =>
            (response.body \ isbnKey).asOpt[JsValue] match {
              case Some(hit) =>
                SearchResults(openLibToSearchResult(hit).toList)
              case _ =>
                console ! Warning("Unexpected json output from OpenLibrary API!")
                SearchResults(Nil)
            }

          case code =>
            console ! Warning("Request to Open Library failed with code: "+code)
            SearchResults(Nil)
        }

      case None =>
        SearchResults(Nil)
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
