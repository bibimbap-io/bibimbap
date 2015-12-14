package io.bibimbap

import bibtex._
import strings._

import scala.concurrent.Future
import akka.actor.ActorRef

import java.io.File

import org.apache.lucene.analysis.standard._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryParser._
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.util.Version

trait LuceneBackend {
  private val JAVAPREFIX = "java-"
  private val LATEXPREFIX = "latex-"

  val console: ActorRef
  val source: String

  private val analyzer = new StandardAnalyzer(Version.LUCENE_36)

  def getLuceneIndex : Directory

  protected var index: Directory = null

  protected var keySet = Set[String]()

  def getNewWriter(): IndexWriter = {
    val config   = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    new IndexWriter(index, config)
  }

  def initializeIndex() = {
    index  = getLuceneIndex
    getNewWriter().close()

    foreachDocument { d =>
      keySet += d.get("__key")
    }
  }

  def foreachDocument(f: Document => Unit) {
    val reader = IndexReader.open(index)
    for (i <- 0 until reader.maxDoc if !reader.isDeleted(i)) {
      f(reader.document(i)) 
    }

    reader.close()
  }

  def containsKey(key: String) = keySet contains key

  def deleteEntryByKey(key: String) {
    val writer = getNewWriter()
    writer.deleteDocuments(new Term("__key", key))
    writer.close()

    keySet -= key
  }

  def searchLucene(query: String, limit: Int): List[SearchResult] =
    searchLuceneRaw(QueryParser.escape(query), limit)

  def searchLuceneRaw(query: String, limit: Int): List[SearchResult] =
    searchEntries(query, limit).flatMap{ case (doc, score) => documentToSearchResult(doc, score) }.toList

  def addEntry(entry: BibTeXEntry): Unit =
    addEntries(List(entry))
    
  def addEntries(entries : Iterable[BibTeXEntry]) : Unit = {
    val writer = getNewWriter()
    for (entry <- entries) {
      if (containsKey(entry.getKey)) {
        writer.deleteDocuments(new Term("__key", entry.getKey))
      }

      val doc = new Document()

      for((k,v) <- entry.entryMap) {
        v.java.foreach { javaStr =>
          doc.add(new Field(JAVAPREFIX + k, javaStr, Field.Store.YES, Field.Index.NO))
        }
        v.latex.foreach { latexStr =>
          doc.add(new Field(LATEXPREFIX + k, latexStr, Field.Store.YES, Field.Index.NO))
        }
      }

      doc.add(new Field("__key",  entry.getKey, Field.Store.YES, Field.Index.NOT_ANALYZED))
      doc.add(new Field("__type", entry.tpe.map(_.toString).getOrElse(""), Field.Store.YES, Field.Index.NO))

      val sb = new StringBuilder()
      entry.title.foreach(t => sb.append(t.toJava))
      sb.append(" ")
      entry.authors.foreach { author =>
        sb.append(author.toJava)
        sb.append(" ")
      }
      entry.journal.foreach(j => sb.append(j.toJava))
      entry.booktitle.foreach(b => sb.append(b.toJava))
      entry.year.foreach(y => sb.append(y.toJava))

      doc.add(new Field("__blob", sb.toString, Field.Store.NO, Field.Index.ANALYZED))

      writer.addDocument(doc)
    }

    keySet = keySet ++ entries.map(_.getKey)

    writer.close()
  }

  private def searchEntries(query : String, limit: Int) : Iterable[(Document, Double)] = {
    val q = new QueryParser(Version.LUCENE_36, "__blob", analyzer).parse(query)
    val reader = IndexReader.open(index)
    val searcher = new IndexSearcher(reader)
    val collector = TopScoreDocCollector.create(limit, true)
    searcher.search(q, collector)
    val hits : Array[ScoreDoc] = collector.topDocs.scoreDocs
    val docs = hits.map(hit => (searcher.doc(hit.doc), hit.score.toDouble))
    searcher.close()
    docs
  }

  def documentToEntry(document: Document): Option[BibTeXEntry] = {
    import scala.collection.JavaConversions._

    var em : Map[String,MString] = Map.empty

    for(f <- document.getFields() if f.name.startsWith(JAVAPREFIX) || f.name.startsWith(LATEXPREFIX)) {
      val (fn, ms) = if(f.name.startsWith(JAVAPREFIX)) {
        val fn = f.name.substring(JAVAPREFIX.length)
        val ms = em.get(fn).map { s =>
          s.copy(java = Some(f.stringValue))
        } getOrElse {
          MString.fromJava(f.stringValue)
        }
        (fn, ms)
      } else {
        val fn = f.name.substring(LATEXPREFIX.length)
        val ms = em.get(fn).map { s =>
          s.copy(latex = Some(f.stringValue))
        } getOrElse {
          MString.fromLaTeX(f.stringValue)
        }
        (fn, ms)
      }

      em = em.updated(fn, ms)
    }

    val optKey = document.get("__key") match {
      case null => None
      case ""   => None
      case s    => Some(s)
    }

    val kind = BibTeXEntryTypes.withNameOpt(document.get("__type"))

    BibTeXEntry.fromEntryMap(kind, optKey, em, console ! Error(_))
  }

  private def documentToSearchResult(document : Document, score: Double) : Option[SearchResult] = {
    documentToEntry(document).map(entry => SearchResult(entry, Set(source), score))
  }

  def clear() = {
    initializeIndex()
  }

}

trait LuceneRAMBackend extends LuceneBackend {

  def getLuceneIndex = new RAMDirectory
}

trait LuceneHDDBackend extends LuceneBackend {

  protected val cacheDir: File

  override def clear() = {
    import org.apache.commons.io.FileUtils
    import java.io.IOException
    try {
      FileUtils.deleteDirectory(cacheDir)
      initializeIndex()
    } catch {
      case ioe : IOException =>
        console ! Warning(ioe.getLocalizedMessage)
    }
  }

  def getLuceneIndex = FSDirectory.open(cacheDir)
}

trait LuceneSearchProvider extends SearchProvider {
  this: LuceneBackend =>

  override def preStart() {
    initializeIndex()
  }

  override def search(terms: List[String], limit: Int): Future[SearchResults] = {
    val query = terms.mkString(" ").trim
    if(query.isEmpty) {
      Future(SearchResults(Nil))
    } else {
      Future(SearchResults(searchLucene(query, limit)))
    }
  }

}
