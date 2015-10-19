package io.bibimbap

import scala.reflect.ClassTag
import akka.actor._
import akka.actor.SupervisorStrategy._
import akka.pattern.ask
import akka.routing.RoundRobinRouter
import akka.util.Timeout
import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class Repl(homeDir: String, configFileName: String, historyFileName: String) extends Actor with ActorHelpers {
  val settings = (new ConfigFileParser(configFileName)).parse.getOrElse(DefaultSettings)

  val console = context.actorOf(Props(new Console(self, settings, historyFileName)), name = "Console")

  var modules = Map[String, ActorRef]()

  override def preStart = {
    sayHello()

    startModules()
  }

  def startModules() {
    import io.bibimbap.modules._

    val managed      = context.actorOf(Props(new Managed(self, console, settings)),      name = "managed");

    val searchSources = List(
      SearchSource(managed, "managed file"),
      SearchSource(context.actorOf(Props(new SearchLocal(self, console, settings)), name = "searchLocal"), "local cache"),
      SearchSource(context.actorOf(Props(new SearchDBLP(self, console, settings)), name = "searchDBLP"), "DBLP"),
      SearchSource(context.actorOf(Props(new SearchOpenLibrary(self, console, settings)), name = "searchOpenLibrary"), "Open Library")
    )

    modules = Map(
      "general"     -> context.actorOf(Props(new General(self, console, settings)),                 name = "general"),
      "search"      -> context.actorOf(Props(new Search(self, console, settings, searchSources)),   name = "search"),
      "results"     -> context.actorOf(Props(new ResultStore(self, console, settings)),             name = "results"),
      "wizard"      -> context.actorOf(Props(new Wizard(self, console, settings)),                  name = "wizard"),
      "consolidate" -> context.actorOf(Props(new Consolidate(self, console, settings)),             name = "consolidate"),
      "managed"     -> managed
    )

  }

  override val supervisorStrategy =
      OneForOneStrategy(maxNrOfRetries = 0, withinTimeRange = 1.minute) {
        case _: RuntimeException         ⇒ Stop
        case _: Exception                ⇒ Escalate
      }

  def dispatchCommand(cmd: Command) {
    implicit val timeout = Timeout(10.hours)

    val results = dispatchMessage[CommandResult](cmd, modules.values.toList) 

    val isKnown = results exists {
      case CommandSuccess  =>
        true
      case CommandException(e)    =>
        println("WOOT")
        console ! Error(e.getMessage)
        true
      case CommandError(msg)      =>
        println("WOOTa")
        console ! Error(msg)
        true

      case _ =>
        false
    }

    if (!isKnown && !results.isEmpty) {
      cmd match {
        case InputCommand(line) =>
          console ! Error("Unknown command: "+line)
        case _ =>
          console ! Error("Unknown command type: "+cmd)
      }
    }
  }

  def receive = {
    case Start =>
      dispatchCommand(OnStartup(modules = modules))
      syncCommand(console, OnStartup(modules = modules))
      self ! ReadLine()

    case ReadLine(_, _) =>
      implicit val timeout = neverTimeout

      console ! Out("")
      syncMessage[LineRead](console, ReadLine()) match {
        case Some(EOF) =>
          // TODO: find a better way to exit
          sys.exit(0)
        case Some(LineRead(line)) =>
          console ! Out("")
          val cmd = line.trim
          if(cmd != "") {
            dispatchCommand(InputCommand(cmd))
          }
        case _ =>
      }

      self ! ReadLine()
    case Shutdown =>
      dispatchCommand(OnShutdown())
      console ! Out("Bye.")
      // TODO: find a better way to exit
      sys.exit(0)
  }

  private def sayHello() {
    console ! Out("""         __    _ __    _           __                        """)
    console ! Out("""   ———  / /_  (_) /_  (_)___ ___  / /_  ____ _____  ——————   """)
    console ! Out("""  ———  / __ \/ / __ \/ / __ `__ \/ __ \/ __ `/ __ \  ————    """)
    console ! Out(""" ———  / /_/ / / /_/ / / / / / / / /_/ / /_/ / /_/ /  ———     """)
    console ! Out("""———  /_.___/_/_.___/_/_/ /_/ /_/_.___/\__,_/ .___/  ———      """)
    console ! Out("""                                          /_/         비빔밥 """)
    console ! Out("")
  }
}
