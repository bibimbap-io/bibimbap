package io.bibimbap
package modules

import akka.actor._
import bibtex._
import strings._
import identifiers.DOI

import play.api.libs.json._
import scalaj.http._
import scala.io.Source

class SearchDBLP(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends SearchProvider {
  val name   = "Search DBLP"
  val source = "dblp"

  private val apiURL     = "http://dblp.uni-trier.de/search/publ/api"
  private val apiBibURL  = "http://dblp.uni-trier.de/rec/bib2/%s.bib"

  override def search(terms: List[String], limit: Int): SearchResults = {

    val request = Http(apiURL).param("q", terms.mkString(" "))
                              .param("h", limit.toString)
                              .param("format", "json");

    val response = request.timeout(connTimeoutMs = 1000, readTimeoutMs = 2000)
                          .execute(parser = { is => Json.parse(is) })

    response.code match {
      case 200 =>
        val hits = (response.body \ "result" \ "hits" \ "@total").asOpt[String]

        hits match {
          case Some(i) if i != "0" =>
            (response.body \ "result" \ "hits" \ "hit").asOpt[List[JsValue]] match {
              case Some(hits) =>
                SearchResults(hits.flatMap(dblpToSearchResult))
              case _ =>
                console ! Warning("Unexpected json output from DBLP API!")
                SearchResults(Nil)
            }

          case _ =>
            SearchResults(Nil)
        }

      case code =>
        console ! Warning("Request to DBLP failed with code: "+code)
        SearchResults(Nil)
    }
  }

  private def dblpToSearchResult(record: JsValue): Option[SearchResult] = {
    val score = (record \ "@score").asOpt[String].map(_.toInt/200d).getOrElse(0d)

    val dblpID = (record \ "@id").asOpt[String]

    val ourl = (record \ "info" \ "url").asOpt[String]

    ourl match {
      case Some(url) if url.startsWith("http://dblp.org/rec/") =>
        val key = url.stripPrefix("http://dblp.org/rec/")

        val bibURL = apiBibURL.format(key)

        val bibResponse = Http(bibURL).asString

        bibResponse.code match {
          case 200 =>
            val bib = bibResponse.body

            val parser = new BibTeXParser(Source.fromString(bib), console ! Warning(_))

            parser.entries.toList match {
              // This means we could parse at least one entry. It could have
              // been two, since DBLP shows two entries for conference
              // proceedings,
              // but our BibTeX parser inlines the relevant fields from the
              // second one into the first one anyway.
              case entry :: _ =>

                // ...and now, a hack to shorten to LNCS, etc.
                val entry2 = entry.fields.get("series").map { ms =>
                  val newSeries = ms.toJava match {
                    case "Lecture Notes in Computer Science"        => MString.fromJava("LNCS")
                    case "Lecture Notes in Artificial Intelligence" => MString.fromJava("LNAI")
                    case _ => ms
                  }

                  if(newSeries == ms) {
                    entry
                  } else {
                    entry.copy(fields = entry.fields.updated("series", newSeries))
                  }
                } getOrElse {
                  entry
                }

                // Looking for a doi somewhere in the soup.
                val doi = entry2.fields.get("ee").flatMap(ms => DOI.extract(ms.toJava))
                          .orElse(entry2.fields.get("url").flatMap(ms => DOI.extract(ms.toJava)))

                val entry3 = doi.map { d =>
                  entry2.updateField("doi", MString.fromJava(d))
                } getOrElse {
                  entry2
                }


                Some(SearchResult(entry3, Set(source), score))

              case _ =>
                None
            }

          case code =>
            console ! Warning("Request to DBLP .bib file failed with code: "+code)
            None
        }

      case Some(url) =>
        console ! Warning("Unnexpected URL: "+url)
        None
      case None =>
        None
    }
  }
}
