package io.bibimbap

import akka.actor._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait SearchProvider extends Actor {
  val source: String

  implicit val ec = context.system.dispatcher

  def receive: Receive = {
    case Search(terms, limit) =>
      val s = sender


      for (r <- search(terms, limit)) {
        s ! r
      }

    case ImportedResult(res) =>
      onImport(res)
      // no message back

    case _ =>
  }

  def search(terms: List[String], limit: Int): Future[List[SearchResult]]

  def onImport(res: SearchResult) = {}
}
