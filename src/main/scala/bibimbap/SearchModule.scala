package bibimbap

import data._

trait SearchModule extends Module {
  val source: String

  def receive = {
    case Search(terms) =>
      sender ! search(terms)
    case Clear =>
      clear()
    case Store(results) =>
      store(results)
    case _ =>
  }

  def search(terms: List[String]): SearchResults

  def store(results: SearchResults) { }

  def clear() { }
}