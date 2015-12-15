package io.bibimbap

import akka.actor._
import akka.pattern.ask

import scala.concurrent.duration._
import scala.concurrent.Await

import jline.console._
import jline.console.completer._
import jline.console.history._

import collection.JavaConversions._
import java.io.File

class Console(repl: ActorRef, settings: Settings, historyFileName: String) extends Actor {
  implicit val ec = context.system.dispatcher

  var buffer: Option[String] = None

  val reader    = new ConsoleReader()

  val completer = new Completer {
    def complete(buffer: String, pos: Int, results: java.util.List[CharSequence]): Int = {
      val to = 1.seconds

      val completionsF = ask(repl, AutoComplete(buffer, pos))(to).mapTo[AutoCompleted]
      val AutoCompleted(candidates, index) = Await.result(completionsF, to)

      if (candidates.isEmpty) {
        -1
      } else {
        results addAll candidates
        index
      }
    }
  }

  override def preStart() {
    val history = new FileHistory(new File(historyFileName))

    reader.setHistory(history)
    reader.setHistoryEnabled(true)

    reader.addCompleter(completer)
  }

  override def postStop() {
    reader.getTerminal.restore()
    reader.shutdown()
  }

  private val defaultHandle = "bibimbap> "

  def receive = {
    case ReadLine(handle, oline) =>
      buffer = oline
      sender ! LineRead(reader.readLine(handle.getOrElse(defaultHandle)))

    case Out(msg: String) =>
      out(msg)

    case Info(msg: String) =>
      out("  "+msg)

    case Warning(msg: String) =>
      if (settings.colors) {
        out(Console.YELLOW + "  \u27A4" + Console.RESET + "  "+msg)
      } else {
        out("  [warn] "+msg)
      }

    case Error(msg: String) =>
      if (settings.colors) {
        out(Console.RED + "  \u27A4" + Console.RESET + "  "+msg)
      } else {
        out("  [err] "+msg)
      }

    case Success(msg: String) =>
      if (settings.colors) {
        out(Console.GREEN + "  \u27A4" + Console.RESET + "  "+msg)
      } else {
        out("  [ok] "+msg)
      }

  }

  private def out(msg: String) {
    Console.println(msg)
  }
}
