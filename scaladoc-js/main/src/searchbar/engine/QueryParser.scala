package dotty.tools.scaladoc

import scala.util.matching.Regex._
import scala.util.matching._

class QueryParser:
  val kinds = Seq(
    "class",
    "trait",
    "enum",
    "object",
    "def",
    "val",
    "var",
    "package",
    "given",
    "type"
  )
  val kindRegex = ("(?i)" + kinds.mkString("(","|",")") + " (.*)").r
  val nameRegex = raw"(.*)".r
  val escapedRegex = raw"`(.*)`".r
  val signatureRegex = raw"(.*=>.*)".r

  def parseMatchers(query: String): EngineQuery = query match {
    case escapedRegex(rest) => NameAndKindQuery(Some(rest.nn), None)
    case kindRegex(kind, rest) => NameAndKindQuery(Some(rest.nn), Some(kind.nn))
    case nameRegex(name) => NameAndKindQuery(Some(name.nn), None)
    case _ => NameAndKindQuery(None, None)
  }

  def parse(query: String): EngineQuery = query match {
    case signatureRegex(signature) => SignatureQuery(signature.nn)
    case other => parseMatchers(other)
  }
