package io.bibimbap
package modules

import akka.actor._
import bibtex._

import strings._
import util.StringUtils

import identifiers.DOI

import java.net.URLEncoder

import json._
import scala.io.Source

class SearchDBLP(val repl: ActorRef, val console: ActorRef, val settings: Settings) extends SearchProvider with WebProvider {
  val name   = "Search DBLP"
  val source = "dblp"

  private val searchURL  = "http://www.dblp.org/search/api/?q=%s&h=%d&c=4&f=0&format=json"

  override def search(terms: List[String], limit: Int): SearchResults = {
    val pattern = URLEncoder.encode(terms.mkString(" "), "UTF-8")

    HTTPQueryAsString(searchURL.format(pattern, limit)) match {
      case Some(text) =>
        SearchResults(extractJSONRecords(text).flatMap(recordToResult).map(completeRecord).toList)
      case None =>
        SearchResults(Nil)
    }
  }

  private def extractJSONRecords(text : String) : Seq[JValue] = {
    new JSONParser().parseOpt(text) match {
      case Left(error) =>
        console ! Warning("DBLP returned malformed JSON data.")
        console ! Warning(error)
        Seq.empty

      case Right(jvalue) =>
        (jvalue \\ "hit").flatMap(hit => hit match {
          case JArray(elems) => elems
          case single : JObject => Seq(single)
          case _ => Nil
        })
    }
  }

  private val unknown : MString = MString.fromJava("???")

  private val CoRR = """(.*CoRR.*)""".r
  
  // Conference paper entries ("inproceedings")
  private val ConfVenueStr1 = """(.*) (\d\d\d\d):([\d- ]*)""".r
  private val ConfVenueStr2 = """(.*) (\d\d\d\d)""".r

  // Journal entries 
  // e.g. "Commun. ACM (CACM) 55(2):103-111 (2012)"
  // or   "Theor. Comput. Sci. (TCS) 198(1-2):1-47 (1998)"
  private val JourVenueStr1 = """(.*) (\d+)\(([\d- ]+)\):([\d- ]*) \((\d\d\d\d)\)""".r
  // e.g. "Acta Inf. (ACTA) 1:271-281 (1972)"
  private val JourVenueStr2 = """(.*) (\d+):([\d- ]*) \((\d\d\d\d)\)""".r
  // e.g. "Logical Methods in Computer Science (LMCS) 4(4) (2008)"
  private val JourVenueStr3 = """(.*) (\d+)\((\d+)\) \((\d\d\d\d)\)""".r

  // Book entries
  private val BookVenueStr1 = """(.*) (\d\d\d\d)""".r

  // "incollection" entries
  private val InCollectionVenueStr1 = """(.*) (\d\d\d\d):([\d- ]*)""".r

  private def recordToResult(record : JValue) : Option[SearchResult] = {
    def yr2yr(year : Option[String]) : Option[MString] =
      year.map(str => MString.fromJava(str.trim))

    val score = (record \ "@score") match {
      case JInt(x) => x/200d
      case _ => 0d
    }

    val dblpID = (record \ "@id") match {
      case JInt(x) => Some(MString.fromJava(x+""))
      case _ => None
    }

    val optKey = None

    val url = (record \ "url") match {
      case JString(str) =>
        Some(MString.fromJava(str))
      case _ =>
        None
    }

    (record \ "info" ) match {
      case obj : JObject => {
        val authors : MString = MString.fromJava(((obj \ "authors" \ "author") match {
          case JArray(elems) => elems.collect { case JString(str) => str }
          case JString(single) => Seq(single)
          case _ => Nil
        }).mkString(" and "))

        val title : MString = (obj \ "title" \ "text") match {
          case JString(str) => MString.fromJava(cleanupTitle(str))
          case _ => unknown
        }

        // PS, 27.12.2012: it seems DBLP has stopped sending this info.
        val (link, doi) = (obj \ "title" \ "@ee") match {
          case JString(str) => 
            val doi = if (str.startsWith("http://doi.acm.org/")) {
              Some(str.substring("http://doi.acm.org/".length, str.length))
            } else if (str.startsWith("http://dx.doi.org/")) {
              Some(str.substring("http://dx.doi.org/".length, str.length))
            } else {
              None
            }

            (Some(MString.fromJava(str)), doi.map(MString.fromJava))
          case _ =>
            (None, None)
        }

        val year : Option[MString] = (obj \ "year") match {
          case JInt(bigInt) => Some(MString.fromJava(bigInt.toString))
          case JString(yr)  => Some(MString.fromJava(yr))
          case _ => None
        }

        // Some of the info is entry type specific, so we now check the type.
        (obj \ "type") match {
          case JString("inproceedings") => {
            val (venue,venueYear,pages) = (obj \ "venue") match {
              case venueObj : JObject => (
                  (venueObj \ "@conference").asOpt[String].map(cleanupVenue),
                  None,
                  (venueObj \ "@pages").asOpt[String].map(cleanupPages)
              )

              case JString(ConfVenueStr1(v, y, p)) => (Some(cleanupVenue(v)), Some(y), Some(cleanupPages(p)))
              case JString(ConfVenueStr2(v, y)) => (Some(cleanupVenue(v)), Some(y), None)
              case JString(os) => console ! Warning("Could not extract venue information from string [" + os + "]."); (None, None, None)
              case _ => (None, None, None)
            }
            val emap = Map[String, Option[MString]](
                "title"     -> Some(title),
                "author"    -> Some(authors),
                "booktitle" -> venue.map(MString.fromJava),
                "year"      -> yr2yr(venueYear).orElse(year),
                "pages"     -> pages.map(MString.fromJava),
                "link"      -> link,
                "url"       -> url,
                "doi"       -> doi,
                "dblp"      -> dblpID
            ).filterNot(_._2.isEmpty).mapValues(_.get)

            val entry = BibTeXEntry.fromEntryMap(Some(BibTeXEntryTypes.InProceedings), optKey, emap, console ! Error(_))

            entry.map(SearchResult(_, Set(source), score))
          }

          case JString("article") => {
            val (isCoRR,jour,vol,num,pgs,yr) = (obj \ "venue") match {
              case venueObj : JObject => (
                (venueObj \ "text").asOpt[String].exists(_.startsWith("CoRR")),
                (venueObj \ "@journal").asOpt[String].map(cleanupJournal),
                (venueObj \ "@volume").asOpt[String],
                (venueObj \ "@number").asOpt[String],
                (venueObj \ "@pages").asOpt[String].map(cleanupPages),
                None
              )
              case JString(CoRR(_)) => (true, None, None, None, None, None)
              case JString(JourVenueStr1(j,v,n,p,y)) => (false, Some(cleanupJournal(j)), Some(v), Some(n), Some(cleanupPages(p)), Some(y))
              case JString(JourVenueStr2(j,v,p,y)) => (false, Some(cleanupJournal(j)), Some(v), None, Some(cleanupPages(p)), Some(y))
              case JString(JourVenueStr3(j,v,n,y)) => (false, Some(cleanupJournal(j)), Some(v), Some(n), None, Some(y))
              // case JString(os) => warn("Could not extract venue information from string [" + os + "]."); (false, None, None, None, None, None)
              case _ => (false, None, None, None, None, None)
            }

            if(isCoRR) {
              None
            } else {
              val emap = Map[String, Option[MString]](
                "author"    -> Some(authors),
                "title"     -> Some(title),
                "journal"   -> jour.map(MString.fromJava),
                "year"      -> yr2yr(yr).orElse(year),
                "volume"    -> vol.map(MString.fromJava),
                "number"    -> num.map(MString.fromJava),
                "pages"     -> pgs.map(MString.fromJava),
                "link"      -> link,
                "url"       -> url,
                "doi"       -> doi,
                "dblp"      -> dblpID
              ).filterNot(_._2.isEmpty).mapValues(_.get)

              BibTeXEntry.fromEntryMap(Some(BibTeXEntryTypes.Article), optKey, emap, console ! Error(_)).map(SearchResult(_, Set(source), score))
            }
          }

          // e.g. Pierce Types and Programming Languages
          case JString("book") => {
            val (publisher,yr) = (obj \ "venue") match {
              case venueObj : JObject => (
                (venueObj \ "@publisher").asOpt[String],
                None
              )
              case JString(BookVenueStr1(p,y)) => (Some(p), Some(y))
              case _ => (None, None)
            }

            val emap = Map[String, Option[MString]](
              "author"     -> Some(authors),
              "title"      -> Some(title),
              "year"       -> yr2yr(yr).orElse(year),
              "publisher"  -> publisher
            ).filterNot(_._2.isEmpty).mapValues(_.get)

            BibTeXEntry.fromEntryMap(Some(BibTeXEntryTypes.Book), optKey, emap, console ! Error(_)).map(SearchResult(_, Set(source), score))
          }

          case JString("incollection") => {
            // title author booktitle year
            // editor volume number series type chapter pages address edition month note key
            val (bkt,yr,p) = (obj \ "venue") match {
              case JString(InCollectionVenueStr1(b,y,p)) => (Some(b),Some(y),Some(cleanupPages(p)))
              case _ => (None,None,None)
            }

            val emap = Map[String,Option[MString]](
              "author"    -> Some(authors),
              "title"     -> Some(title),
              "year"      -> yr2yr(yr).orElse(year),
              "booktitle" -> bkt.map(MString.fromJava)
            ).filterNot(_._2.isEmpty).mapValues(_.get)

            BibTeXEntry.fromEntryMap(Some(BibTeXEntryTypes.InCollection), optKey, emap, console ! Error(_)).map(SearchResult(_, Set(source), score))
          }

          case _ => None
        }
      }
      case _ => None
    }
  }

  private def completeRecord(res: SearchResult): SearchResult = {
    val urlPref1 = "http://www.dblp.org/rec/bibtex/"
    val urlPref2 = "http://www.dblp.org/rec/bib2/"

    val url: Option[String] = res.entry.url.map(_.toJava).flatMap { s =>
      if(s.startsWith(urlPref1)) {
        Some(urlPref2 + s.substring(urlPref1.length) + ".bib")
      } else {
        None
      }
    }

    url.flatMap(HTTPQueryAsString(_)) match {
      case Some(txt) =>
        val parser = new BibTeXParser(Source.fromString(txt), console ! Warning(_))

        parser.entries.toList match {
          // This means we could parse at least one entry. It could have been two, since DBLP shows two entries for conference proceedings,
          // but our BibTeX parser inlines the relevant fields from the second one into the first one anyway.
          case sndEntry :: _ => 
            val fstEntry = res.entry
            val mgdEntry = res.entry inlineFrom sndEntry

            // We prefer LaTeX format for these, so we pick it from the LaTeX.
            // We drop the url field because this is always a DBLP link.
            val ttlEntry = mgdEntry.drop("url").pickFrom(sndEntry, "title", "author", "editor")

            // ...and now, a hack to shorten to LNCS, etc.
            val fnlEntry = ttlEntry.fields.get("series").map { ms =>
              val newSeries = ms.toJava match { 
                case "Lecture Notes in Computer Science" => MString.fromJava("LNCS")
                case "Lecture Notes in Artificial Intelligence" => MString.fromJava("LNAI")
                case _ => ms
              }

              if(newSeries == ms) {
                ttlEntry
              } else {
                ttlEntry.copy(fields = ttlEntry.fields.updated("series", newSeries))
              }
            } getOrElse {
              ttlEntry
            }

            // Looking for a doi somewhere in the soup.
            val doi = sndEntry.fields.get("ee").flatMap(ms => DOI.extract(ms.toJava)).orElse(sndEntry.fields.get("url").flatMap(ms => DOI.extract(ms.toJava)))
            val withDOI = doi.map { d =>
              fnlEntry.updateField("doi", MString.fromJava(d))
            } getOrElse {
              fnlEntry
            }

            res.copy(entry = withDOI)

          case _ =>
            res
        }
      case None =>
        res
    }
  }

  private val FinalDot = """(.*)\.\s*""".r
  private def cleanupTitle(title : String) : String = {
    val trimmed = title.trim
    val noDot = trimmed match {
      case FinalDot(s) => s
      case other => other
    }

    StringUtils.unescapeHTML(noDot)
  }

  private def cleanupVenue(venue : String) : String = {
    venue.trim
  }

  private lazy val JournalAbbr = """(.*) \(([A-Z]+)\)""".r
  private def cleanupJournal(journal : String) : String = {
    journal.trim match {
      case JournalAbbr(_, abbr) => abbr
      case other => other
    }
  }

  private val Pages = """(\d+)([\s-]+)(\d+)""".r
  private def cleanupPages(pages : String) : String = pages.trim match {
    case Pages(start, _, end) => start + "--" + end
    case other => other
  }
}
