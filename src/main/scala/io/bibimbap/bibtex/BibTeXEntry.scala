package io.bibimbap
package bibtex

import io.bibimbap.strings._

case class InconsistentBibTeXEntry(msg: String) extends Exception(msg)

// This datatypes and all the following ones assume crossrefs have been
// "resolved" into all entries.
case class BibTeXEntry(tpe: Option[BibTeXEntryTypes.BibTeXEntryType],
                       key: Option[String],
                       fields: Map[String, MString],
                       seqFields: Map[String, Seq[MString]]) extends Serializable {
  lazy val requiredFields = BibTeXEntryTypes.requiredFieldsFor(tpe)
  lazy val optionalFields = BibTeXEntryTypes.optionalFieldsFor(tpe)
  lazy val stdFields      = BibTeXEntryTypes.relevantFieldsFor(tpe)

  // convenience fields
  val address      : Option[MString] = fields.get("address")
  val annote       : Option[MString] = fields.get("annote")
  val authors      : Seq[MString]    = seqFields.getOrElse("author", Seq.empty)
  val booktitle    : Option[MString] = fields.get("booktitle")
  val chapter      : Option[MString] = fields.get("chapter")
  val edition      : Option[MString] = fields.get("edition")
  val editors      : Seq[MString]    = seqFields.getOrElse("editor", Seq.empty)
  val eprint       : Option[MString] = fields.get("eprint")
  val howpublished : Option[MString] = fields.get("howpublished")
  val institution  : Option[MString] = fields.get("institution")
  val journal      : Option[MString] = fields.get("journal")
  val month        : Option[MString] = fields.get("month")
  val note         : Option[MString] = fields.get("note")
  val number       : Option[MString] = fields.get("number")
  val organization : Option[MString] = fields.get("organization")
  val pages        : Option[MString] = fields.get("pages")
  val publisher    : Option[MString] = fields.get("publisher")
  val school       : Option[MString] = fields.get("school")
  val series       : Option[MString] = fields.get("series")
  val title        : Option[MString] = fields.get("title")
  val trType       : Option[MString] = fields.get("trType")
  val url          : Option[MString] = fields.get("url")
  val volume       : Option[MString] = fields.get("volume")
  val year         : Option[MString] = fields.get("year")
  val link         : Option[MString] = fields.get("link")
  val doi          : Option[MString] = fields.get("doi")
  val dblp         : Option[MString] = fields.get("dblp")
  val keyField     : Option[MString] = fields.get("key")

  lazy val entryMap = {
    fields ++ seqFields.mapValues(seq => MString.conjoin(seq, MString.and))
  }

  val allFields = fields.keySet ++ seqFields.keySet

  def updateField(field : String, value : MString) : BibTeXEntry = {
    copy(fields = fields.updated(field, value))
  }

  def inlineFrom(xref: BibTeXEntry): BibTeXEntry = {
    var newFields    = fields
    var newSeqFields = seqFields

    for (field <- stdFields) {
      if (!fields.contains(field) && xref.fields.contains(field)) {
        newFields += field -> xref.fields(field)
      }
      if (!seqFields.contains(field) && xref.seqFields.contains(field)) {
        newSeqFields += field -> xref.seqFields(field)
      }
    }

    copy(fields = newFields, seqFields = newSeqFields)
  }

  def pickFrom(other : BibTeXEntry, fs : String*) : BibTeXEntry = {
    var newFields    = fields
    var newSeqFields = seqFields

    for(field <- fs) {
      other.fields.get(field).foreach { v =>
        newFields += (field -> v)
      }

      other.seqFields.get(field).foreach { vs =>
        newSeqFields += (field -> vs)
      }
    }

    copy(fields = newFields, seqFields = newSeqFields)
  }

  def drop(fs : String*) : BibTeXEntry = {
    val asSet = fs.toSet
    if(!asSet.exists(allFields(_))) {
      this
    } else {
      copy(fields = fields -- asSet, seqFields = seqFields -- asSet)
    }
  }

  // Checks whether a bibtexentry may be the same with another
  def like(that: BibTeXEntry): Boolean = {
    def compField(a: Option[MString], b: Option[MString]) = (a,b) match {
      case (Some(aa), Some(bb)) =>
        aa.toJava == bb.toJava
      case _ =>
        false
    }

    if (this == that) {
      true
    } else if (compField(this.doi, that.doi)) {
      true
    } else if (compField(this.dblp, that.dblp)) {
      true
    } else if (this.getKey == that.getKey) {
      true
    } else if (this.generateKey == that.generateKey) {
      true
    } else if (compField(this.title, that.title)) {
      // Let's make sure by checking another criteria
      compField(this.year, that.year) ||
      compField(this.journal, that.journal) ||
      compField(this.booktitle, that.booktitle)
    } else {
      false
    }
  }

  def isValid: Boolean = {
    val missingReqFields = requiredFields.filter(!_.satisfiedBy(allFields))

    missingReqFields.isEmpty
  }

  def getType: BibTeXEntryTypes.BibTeXEntryType = tpe.getOrElse(BibTeXEntryTypes.Misc)
  def getKey: String = key.getOrElse(generateKey)

  private val FourDigits = """.*(\d{4}).*""".r
  def generateKey: String = {
    val commonWords = Set("", "in", "the", "a", "an", "of", "for", "and", "or", "by", "on", "with")

    def isBibTeXFriendly(c : Char) : Boolean = (
      (c >= 'A' && c <= 'Z') ||
      (c >= 'a') && (c <= 'z') ||
      (c >= '0') && (c <= '9')
    )

    def camelcasify(str : MString) : Seq[String] = {
      str.toJava.split(" ")
        .map(bit => MString.javaToASCII(bit).filter(isBibTeXFriendly))
        .filterNot(_.isEmpty)
        .map(_.toLowerCase)
        .filterNot(commonWords)
        .map(_.capitalize)
    }

    def lastFromPerson(person : MString) : String = {
      val lastBit = MString.fromJava(person.toJava.split(" ").last)
      lastBit.toASCII.filter(isBibTeXFriendly)
    }

    val persons   = if(!authors.isEmpty) authors else editors
    val lastnames = if(persons.size > 3) {
      lastFromPerson(persons(0)) + "ETAL"
    } else {
      persons.map(lastFromPerson).mkString("")
    }

    val yr = year match {
      case Some(y) => y.toJava match {
        case FourDigits(ds) =>
          val last = ds.toJava.toInt % 100
          if(last < 10) "0" + last else last.toString

        case _ => ""
      }
      case None => ""
    }

    val title = this.title.map(t =>
      camelcasify(t).take(6).mkString("")
    ).getOrElse("")

    lastnames + yr + title
  }

  // Tries to shorten the first names (whatever that means).

  def inlineString: String = {

    def shortenName(name : String) : String = {
      val elements = name.split(" ").filterNot(_.isEmpty)
      if (elements.size > 1) {
        elements.dropRight(1).map(e => e(0) + ".").mkString("") + elements.last
      } else {
        name
      }
    }

    val (persons,areEditors) = if(!authors.isEmpty) {
      (authors, false)
    } else {
      (editors, true)
    }

    val personString = if(persons.size > 4) {
      shortenName(persons.head.toJava) + " et al."
    } else {
      persons.map(p => shortenName(p.toJava)).mkString(", ")
    }

    val names = if(areEditors) (personString + " ed.") else personString

    val title = "\"" + this.title.map(_.toJava).getOrElse("?") + "\""

    val where =
      booktitle.map(_.toJava).getOrElse(
        journal.map(_.toJava).getOrElse(
          school.map(_.toJava).getOrElse(
            publisher.map(_.toJava).getOrElse(
              howpublished.map(_.toJava).getOrElse("?")))))

    val year = this.year.map(_.toJava).getOrElse("?")

    names + ", " + title + ", " + where + ", " + year
  }

  override def toString = toStringWithKey(getKey)

  private val preferredDisplayingOrder : Seq[String] = List("title", "author", "editor", "booktitle", "journal", "year")
  def toStringWithKey(key : String) : String = {
    val buffer = new StringBuilder
    buffer.append("@" + getType + "{" + key + ",\n")

    def printOptField(name : String, value : Option[MString]) {
      value.foreach(content => {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(content.toLaTeX)
        buffer.append("},\n")
      })
    }

    def printSeqField(name : String, values : Seq[MString]) {
      if(!values.isEmpty) {
        buffer.append("  ")
        buffer.append("%12s = {".format(name))
        buffer.append(MString.conjoin(values, MString.and).toLaTeX)
        buffer.append("},\n")
      }
    }

    var remaining : Set[String] = allFields
    def printSubset(subset : Traversable[String]) {
      for(field <- subset if remaining(field)) {
        if(seqFields contains field) {  
          printSeqField(field, seqFields(field))
        } else {
          printOptField(field, fields.get(field))
        }
        remaining -= field
      }
    }

    printSubset(preferredDisplayingOrder)
    printSubset(requiredFields.flatMap(_.toFields).toSeq.sorted)
    printSubset(optionalFields.toSeq.sorted)
    printSubset(allFields.toSeq.sorted)

    buffer.dropRight(2).append("\n}").toString
  }

  def display(out: String => Unit, fieldFormatter: String => String, errorFormatter: String => String) {
    val missingVal = ""

    out("  Entry type : "+tpe.getOrElse(missingVal))
    out("  Entry key  : "+getKey)
    out("")
    out("  Required fields:")
    for (f <- requiredFields.flatMap(_.toFields)) {
      if (entryMap contains f) {
        out(("   "+fieldFormatter("%12s")+" = %s").format(f, entryMap(f).toJava))
      } else {
        out(("   "+errorFormatter("%12s")+" = %s").format(f, missingVal))
      }
    }
    out("")
    out("  Optional fields:")
    for (f <- optionalFields) {
      out(("   "+fieldFormatter("%12s")+" = %s").format(f, entryMap.get(f).map(_.toJava).getOrElse(missingVal)))
    }

    val extraFields = entryMap.keySet -- BibTeXEntryTypes.allStdFields
    if (!extraFields.isEmpty) {
      out("")
      out("  Extra fields:")
      for (f <- extraFields) {
        out(("   "+fieldFormatter("%12s")+" = %s").format(f, entryMap(f).toJava))
      }
    }
  }

  /** Attempts to provide an URL for the entry, first by searching
    * for relevant fields, then by falling back to DOI. */
  lazy val getURL : Option[String] = {
    val realURLField =
      fields.get("url").orElse(
        fields.get("link").orElse(
          fields.get("ee")
        )
      )

    realURLField.map(_.toJava).orElse {
      fields.get("doi").map(doi => "http://dx.doi.org/" + doi.toJava)
    }
  }
}

object BibTeXEntry {
  def fromEntryMap(tpe: Option[BibTeXEntryTypes.BibTeXEntryType],
                   key: Option[String],
                   map : Map[String,MString],
                   onError: String => Unit) : Option[BibTeXEntry] = {
    try {
      val isSeqField = Set("author", "editor")

      var fields    = Map[String, MString]()
      var seqFields = Map[String, Seq[MString]]()

      for ((field, value) <- map) {
        if (isSeqField(field)) {
          seqFields += field -> value.split(MString.and)
        } else {
          fields += field -> value
        }
      }

      Some(BibTeXEntry(tpe, key, fields, seqFields))
    } catch {
      case InconsistentBibTeXEntry(msg) =>
        onError(msg)
        None
    }
  }
}
