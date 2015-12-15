package io.bibimbap

import scala.reflect.ClassTag
import akka.actor._
import akka.actor.SupervisorStrategy._
import akka.pattern.ask
import akka.routing.RoundRobinRouter
import akka.util.Timeout
import java.util.concurrent.CountDownLatch
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import play.api.libs.ws.ning.NingWSClient

import io.bibimbap.modules.{Search=>MSearch, _}


class Repl(latch: CountDownLatch,
           homeDir: String,
           configFileName: String,
           historyFileName: String) extends Actor with ActorHelpers {

  val settings = (new ConfigFileParser(configFileName)).parse.getOrElse(DefaultSettings)

  val console = context.actorOf(Props(new Console(self, settings, historyFileName)), name = "Console")

  var wsClient = NingWSClient()

  val ctx = Context(self, console, settings, wsClient)

  val modules = Map(
    "general"     -> context.actorOf(Props(classOf[General],     ctx), name = "general"),
    "abstract"    -> context.actorOf(Props(classOf[Abstract],    ctx), name = "abstract"),
    "search"      -> context.actorOf(Props(classOf[MSearch],     ctx), name = "search"),
    "results"     -> context.actorOf(Props(classOf[ResultStore], ctx), name = "results"),
    "wizard"      -> context.actorOf(Props(classOf[Wizard],      ctx), name = "wizard"),
    "consolidate" -> context.actorOf(Props(classOf[Consolidate], ctx), name = "consolidate"),
    "managed"     -> context.actorOf(Props(classOf[Managed],     ctx), name = "managed")
  )

  var modulesInitialized = Set[String]()

  override def preStart = {
    console ! Out("""         __    _ __    _           __                        """)
    console ! Out("""   ———  / /_  (_) /_  (_)___ ___  / /_  ____ _____  ——————   """)
    console ! Out("""  ———  / __ \/ / __ \/ / __ `__ \/ __ \/ __ `/ __ \  ————    """)
    console ! Out(""" ———  / /_/ / / /_/ / / / / / / / /_/ / /_/ / /_/ /  ———     """)
    console ! Out("""———  /_.___/_/_.___/_/_/ /_/ /_/_.___/\__,_/ .___/  ———      """)
    console ! Out("""                                          /_/         비빔밥 """)
    console ! Out("")
  }

  def receive = {
    case Start =>
      for (m <- modules.values) {
        m ! InitializeModule(modules)
      }

    case ModuleInitialized(name) =>
      if (modules contains name) {
        modulesInitialized += name
      } else {
        console ! Error("Unknown module "+name+" ?!")
      }

      if (modulesInitialized == modules.keySet) {
        for (m <- modules.values) {
          m ! Start
        }

        self ! ReadLine()
      }

    case ac: AutoComplete =>
      val s = sender

      val completions = modules.values.map(m => (m ? ac).mapTo[AutoCompleted])

      for (r <- Future.sequence(completions)) {
        // We compose results from multiple modules
        val candidates = r.map(_.candidates).flatten.toList
        val index = r.map(_.index).min

        s ! AutoCompleted(candidates, index)
      }

    case ReadLine(_, _) =>
      console ! Out("")
      console ! ReadLine()

    case LineRead(line) =>
      console ! Out("")
      val cmd = line.trim

      if (Set("exit", "quit") contains cmd) {
        self ! Shutdown
      } else if (cmd != "") {

        val cmds = for (m <- modules.values) yield {
          ask(m, InputCommand(cmd)).mapTo[CommandResult] 
        }

        for {
          rs <- Future.sequence(cmds)
        } yield {
          if (rs.forall(_ == CommandUnknown)) {
            console ! Error("Unknown command: "+line)
          }

          self ! ReadLine()
        }
      } else {
        self ! ReadLine()
      }

    case Shutdown =>
      console ! Success("So long, sucker!")
      console ! Out("")

      wsClient.close

      latch.countDown()
  }
}
