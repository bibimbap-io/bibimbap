package bibimbap

case class SearchResult(entry: data.BibTeXEntry, link: Option[String], sources: Set[String])
