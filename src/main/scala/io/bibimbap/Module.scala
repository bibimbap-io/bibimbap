package io.bibimbap

import scala.reflect.ClassTag
import akka.actor._
import akka.pattern.ask
import scala.concurrent.Await
import scala.concurrent.duration._

import play.api.libs.ws.ning.NingWSClient

trait Module extends Actor with ActorHelpers {
  val ctx: Context

  val settings: Settings     = ctx.settings
  val console: ActorRef      = ctx.console
  val repl: ActorRef         = ctx.repl
  val wsClient: NingWSClient = ctx.wsClient

  val name: String

  var modules   = Map[String, ActorRef]()

  def receive: Receive = {
    case Command2("help", command) =>
      if (helpItems contains command) {
        helpItems(command).display(console)
      }
      sender ! CommandProcessed

    case Command1("help" | "?") =>
      for ((command, hi) <- helpItems) {
        helpItems(command).displayShort(console)
      }
      sender ! CommandProcessed

    case InitializeModule(mods) =>
      modules = mods
      preInitialization()
      sender ! ModuleInitialized(name)

    case Start =>
      postInitialization()

    case AutoComplete(buffer, pos) =>
      val (res, index) = completeWithHelp(buffer, pos)
      sender ! AutoCompleted(res, index)

    case InputCommand(line) =>
      var foundPartial = false
      for (he <- helpItems.keySet.toSeq.sortBy(-_.length) if !foundPartial && (line startsWith he)) {
        val hi = helpItems(he)
        console ! Error("Incorrect use of "+he+":")
        console ! Error("Usage: "+hi.command+": "+hi.short)

        foundPartial = true;
      }
      if (foundPartial) {
        sender ! CommandProcessed
      } else {
        sender ! CommandProcessed
      }

    case _ =>
      sender ! CommandUnknown
  }

  def preInitialization() = {

  }

  def postInitialization() = {

  }

  def complete(buffer: String, pos: Int): (List[String], Int) = {
    (Nil, 0)
  }

  def completeWithHelp(buffer: String, pos: Int): (List[String], Int) = {
    val toComplete = buffer.substring(0, pos)

    val results = for (command <- helpItems.keySet.toList if command.startsWith(toComplete)) yield {
      command+" "
    }

    if (results.isEmpty) {
      complete(buffer, pos)
    } else {
      (results, 0)
    }
  }

  val helpItems: Map[String, HelpEntry];
}
