package io.bibimbap
package modules

import akka.actor._
import bibtex._
import strings._

import java.io.File

class SearchLocal(val ctx: Context) extends SearchProvider with LuceneHDDBackend with LuceneSearchProvider {
  val name = "SearchLocal"

  val source = "cache"

  protected val cacheDir = new File(ctx.settings("general", "dir.cache"))

  override def onImport(res: SearchResult) {
    addEntry(res.entry)
  }
}
