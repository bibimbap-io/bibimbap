package io.bibimbap

import akka.actor._
import jline.console._
import jline.console.completer._
import jline.console.history._
import java.io.{File, FileInputStream, FileDescriptor, PrintWriter, OutputStreamWriter}

class Console(repl: ActorRef, settings: Settings, historyFileName: String) extends Actor with ActorHelpers {
  val console = self

  private def out(msg: String) {
    Console.println(msg)
  }

  var modules = List[ActorRef]()

  var buffer: Option[String] = None

  val reader    = new ConsoleReader()


  private def magicTrick(): Unit = buffer match {
    case Some(b) =>
      reader.putString(b)
      buffer = None
    case _ =>
  }

  val completer = new Completer {
    def complete(buffer: String, pos: Int, results: java.util.List[CharSequence]): Int = {
      import collection.JavaConversions._

      val resultsList = results.asInstanceOf[java.util.List[String]]

      var ind = -1

      for (Completed(candidates, index) <- dispatchMessage[Completed](Complete(buffer, pos), modules) if candidates != Nil) {
        if (ind == -1) {
          ind = index
        }

        if (index == ind) {
          resultsList addAll candidates
        }
      }

      ind
    }
  }

  override def preStart() {
    val history = new FileHistory(new File(historyFileName))
    reader.setHistory(history)
    reader.setHistoryEnabled(true)

    reader.addCompleter(completer)
  }

  private val defaultHandle = "bibimbap> "

  def receive = {
    case OnStartup(modules) =>
      this.modules = modules.values.toList
      sender ! CommandSuccess

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
}
