package io.bibimbap

import akka.actor._
import play.api.libs.ws.ning.NingWSClient

case class Context(
  repl: ActorRef,
  console: ActorRef,
  settings: Settings,
  wsClient: NingWSClient
)


