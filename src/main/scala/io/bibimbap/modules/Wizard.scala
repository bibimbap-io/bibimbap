package io.bibimbap
package modules

import akka.actor._
import bibtex._
import strings._

class Wizard(val ctx: Context) extends Module {
  val name = "wizard"

  lazy val resultsModule = modules("results")

  override def receive: Receive = {
    case CommandL("new", args) if args.size <= 1 =>
      doAdd(args.headOption.map(BibTeXEntryTypes.withNameOpt(_)).flatten) match {
        case Some(e) =>
          val res = SearchResult(e, Set("add"), relevance = 1, isEdited = true)

          console ! Out("")
          console ! Success("New entry created!")
          console ! Out("")

          syncCommand(resultsModule, SearchResults(List(res)))
        case _ =>
      }

      sender ! CommandProcessed
    case Command2("edit", Indices(ids)) =>
      syncMessage[SearchResults](resultsModule, GetResults(ids)) match {
        case Some(SearchResults(res)) =>
          val newResults = res.map(doEdit)

          syncCommand(resultsModule, ReplaceResults(ids, newResults))
        case _ =>
          console ! Error("Invalid index")
      }

      sender ! CommandProcessed

    case x =>
      super.receive(x)
  }

  private def getFieldValue(field: String, isReq: Boolean = false, value: Option[String] = None): Option[String] = {
    val fieldFormatted = if (isReq)
        inBold("%18s "+settings.RED+"*").format(field)
      else
        inBold("%20s").format(field)

    syncMessage[LineRead](console, ReadLine(Some(fieldFormatted+" = "), value)) match {
      case Some(LineRead("")) | Some(EOF) | None =>
        None
      case Some(LineRead(value)) =>
        Some(value)
    }
  }

  private def inBold(str: String): String    = settings.BOLD+str+settings.RESET
  private def inRedBold(str: String): String = settings.BOLD+settings.RED+str+settings.RESET

  def doAdd(tpe: Option[BibTeXEntryTypes.BibTeXEntryType]): Option[BibTeXEntry] = {

    if (!tpe.isEmpty) {
      console ! Out("Entering bibtex entry: "+tpe.get)
    }

    editFields(tpe, Map(), None)
  }

  private def displayEntry(entry: BibTeXEntry) {
    entry.display(console ! Out(_), inBold, inRedBold)
  }

  def editFields(initType: Option[BibTeXEntryTypes.BibTeXEntryType],
                 initFields: Map[String, MString],
                 initKey: Option[String]): Option[BibTeXEntry] = {

    var tpe              = initType 
    var fields           = initFields
    val allStdFields     = BibTeXEntryTypes.allStdFields

    tpe match {
      case None =>
       tpe = BibTeXEntryTypes.withNameOpt(getFieldValue("entry type", false, None))
      case _ =>
    }
    val key = getFieldValue("entry key", false, initKey)

    for (reqFields <- BibTeXEntryTypes.requiredFieldsFor(tpe); field <- reqFields.toFields) {
      val otherFields = fields - field
      val isStillReq  = !reqFields.satisfiedBy(otherFields.keySet)

     getFieldValue(field, isStillReq, fields.get(field).map(_.toLaTeX)) match {
        case Some(v) =>
          fields += field -> MString.fromLaTeX(v)
        case _ =>
      }
    }

    for (field <- BibTeXEntryTypes.optionalFieldsFor(tpe)) {
      getFieldValue(field, false, fields.get(field).map(_.toLaTeX)) match {
        case Some(v) =>
          fields += field -> MString.fromLaTeX(v)
        case _ =>
      }
    }

    BibTeXEntry.fromEntryMap(tpe, key, fields, console ! Error(_))
  }
  
  def doEdit(res: SearchResult): SearchResult = {

    editFields(res.entry.tpe, res.entry.entryMap, Some(res.entry.getKey)) match {
      case Some(entry) =>
        if (entry != res.entry) {
          console ! Success("Entry modified!")
          res.copy(entry = entry, isEdited = true, oldEntry = res.oldEntry.orElse(Some(res.entry)))
        } else {
          console ! Success("Entry not modified!")
          res
        }
      case None =>
        console ! Error("Could not save entry :(")
        res
    }
  }

  val helpItems = Map(
    "edit"   -> HelpEntry("edit <result>",  "Edit the <results>th item from the last search result."),
    "new"    -> HelpEntry("new <kind>",    "Add a new entry of type <kind>.")
  )
}
