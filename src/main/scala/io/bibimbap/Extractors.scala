package io.bibimbap

abstract class ExCommand(val limit: Int) {
  def unapplySeq(cmd: InputCommand): Option[Seq[String]] = {
    Some(cmd.line.split("[:,. ]", limit).toList)
  }
}

object CommandL {
  def unapply(cmd: InputCommand): Option[(String, List[String])] = {
    val wrds = cmd.line.split("[:,. ]", 0).toList

    if (wrds.size > 0) {
      Some(wrds.head, wrds.tail)
    } else {
      None
    }
  }
}

object Command1 extends ExCommand(1)
object Command2 extends ExCommand(2)
object Command3 extends ExCommand(3)
object Command4 extends ExCommand(4)

private object Regexes {
  val Range     = """(\d+)-(\d+)""".r
  val CSIndices = """(\d+(?:\s*,\s*\d+)*)""".r
}


abstract class Indices {
  def within[T](lst: List[T]): Option[List[T]];
}

object AllIndices extends Indices {
  def within[T](lst: List[T]): Option[List[T]] = {
    if (lst.isEmpty) {
      None
    } else {
      Some(lst)
    }
  }
}

case class SelectedIndices(indices: Traversable[Int]) extends Indices {
  def within[T](lst: List[T]): Option[List[T]] = {
    if (indices.forall(i => i >= 0 && i < lst.size)) {
      Some(indices.map(lst(_)).toList)
    } else {
      None
    }
  }
}

object Indices {
  def unapply(index: String): Option[Indices] = {
    import Regexes._

    index match {
      case "*" =>
        Some(AllIndices)
      case Range(lower, upper) =>
        val l = lower.toInt
        val u = upper.toInt
        Some(SelectedIndices(l to u))
      case CSIndices(str) =>
        Some(SelectedIndices(str.split(",").map(_.trim.toInt)))
      case _ =>
        None
    }
  }
}

object FileCompletor {
  lazy val jlineCompleter = new jline.console.completer.FileNameCompleter()
}

case class FileCompletor(prefix: String) {

  def unapply(info: (String, Int)): Option[(List[String], Int)] = {
    import collection.JavaConversions._

    val buffer = info._1
    val pos    = info._2

    if (buffer.startsWith(prefix) && pos >= prefix.length) {
      val newbuffer = buffer.substring(prefix.length, buffer.length)
      val newpos    = pos - prefix.length

      val list  = new java.util.ArrayList[CharSequence]()
      val index = FileCompletor.jlineCompleter.complete(newbuffer, newpos, list)

      Some((list.toList.map(_.toString), index + prefix.length))
    } else {
      None
    }
  }
}
