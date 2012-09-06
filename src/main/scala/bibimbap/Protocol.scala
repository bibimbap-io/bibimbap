package bibimbap

import bibtex.BibTeXEntry

import akka.actor.ActorRef

case object Start
case class ReadLine(handle: Option[String] = None, prefill: Option[String] = None)
case class LineRead(str: String)
object EOF extends LineRead(null)

// logger stuff

abstract class LogMsg {
  val msg: String 
};
case class Out(msg: String) extends LogMsg;
case class Error(msg: String) extends LogMsg;
case class Warning(msg: String) extends LogMsg;
case class Info(msg: String) extends LogMsg;
case class Success(msg: String) extends LogMsg;

// Interaction between the REPL and modules
trait Command
case class InputCommand(line: String) extends Command
case class OnStartup(modules: Map[String, ActorRef]) extends Command;
case class OnShutdown() extends Command;
case object Shutdown extends Command;

abstract class CommandResult
case class CommandError(msg: String) extends CommandResult
case class CommandException(e: Throwable) extends CommandResult
case object CommandSuccess extends CommandResult
case object CommandUnknown extends CommandResult

case class Complete(buffer: String, pos: Int) extends Command
case class Completed(candidates: List[String], index: Int) extends Command

// Protocol to/from search module
//  => Search(terms)
//  <= SearchResults(...)
case class Search(terms: List[String])

//  => SearchOne(terms)
//  <= SearchResults(Nil  |  x :: Nil)
case class SearchOne(terms: List[String])

//  => Consolidate(entry)
//  <= Consolidated(newentry)
case class Consolidate(entry: BibTeXEntry)
case class Consolidated(entry: BibTeXEntry)

case class SearchResults(entries: List[SearchResult])


// Protocol from/to results module
// => GetResults(index)
// <= SearchResults(...)
case class GetResults(index: String)

// => ShowResults
// <= CommandSuccess
case class ShowResults(terms: List[String] = Nil) extends Command

// => ReplaceResults(index, results)
// <= CommandSuccess
case class ReplaceResults(index: String, entries: List[SearchResult]) extends Command

// => ImportedResult
// ASYNC
case class ImportedResult(res: SearchResult)
