package io.bibimbap
package modules

import akka.actor._

class General(val ctx: Context) extends Module {
  val name = "general"

  val helpItems = Map(
    "help" -> HelpEntry("help [<command>]", "This help"),
    "exit" -> HelpEntry("exit", "Exits bibimbap"),
    "quit" -> HelpEntry("quit", "Exits bibimbap")
  )
}
