package io.bibimbap

import akka.actor._
import scala.concurrent.Future

trait SearchProvider extends Actor {
  val source: String

  def receive: Receive = {
    case Search(terms, limit) =>
      val s = sender

      for (r <- search(terms, limit)) {
        s ! r
      }

    case ImportedResult(res) =>
      onImport(res)
      // no message back

    case os: OnStartup =>
      try {
        startup(os)
        sender ! CommandSuccess
      } catch {
        case e: Exception =>
          sender ! CommandException(e)
      }

    case _ =>
  }

  def search(terms: List[String], limit: Int): Future[SearchResults]

  def onImport(res: SearchResult) = {}

  def startup(os: OnStartup) = {}
}
