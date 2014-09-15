package io.bibimbap

package object json {
  implicit object JStringExtractor extends JExtractor[String] {
    def extract(j: JValue) : Option[String] = j match {
      case JString(v) => Some(v)
      case _ => None
    }
  }
}
