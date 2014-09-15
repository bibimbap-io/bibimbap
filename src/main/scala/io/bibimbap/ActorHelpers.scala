package io.bibimbap

import scala.reflect.ClassTag
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

trait ActorHelpers extends Actor {
  val fastTimeout: Timeout  = Timeout(5.seconds)
  val neverTimeout: Timeout = Timeout(10.hour)
  implicit val timeout: Timeout = neverTimeout

  val console: ActorRef

  def dispatchMessage[T: ClassTag](cmd: Any, to: List[ActorRef])(implicit timeout: Timeout): List[T] = {
    val futures = to.map(actor => (actor ? cmd).mapTo[T].map(Some(_)).recover {
      case e: java.util.concurrent.TimeoutException =>
        console ! Error("Timeout while waiting on partial results")
        None
      case c: ClassCastException =>
        console ! Error("Unnexpected return value from command: "+c.getMessage)
        None
    })

    try {
      Await.result(Future.sequence(futures), timeout.duration).flatten
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        console ! Error("Timeout while waiting for command result")
        List()
    }
  }


  def syncMessage[T: ClassTag](actor: ActorRef, cmd: Any)(implicit timeout: Timeout): Option[T] = {
    dispatchMessage[T](cmd, List(actor)).headOption
  }

  def syncCommand(actor: ActorRef, cmd: Any)(implicit timeout: Timeout) =
    syncMessage[CommandResult](actor, cmd)
}
