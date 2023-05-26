package aspic.parser

import aspic.framework.{Contrary, Framework, Rule}

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Using}

object BaseParser {
  def get(inputFormat: String): Option[BaseParser] = {
    inputFormat match
      case "simple" => Some(SimpleParser)
      case "aspicp" => Some(AspicpParser)
      case "apx" => Some(ApxParser)
      case _ => None
  }
}

trait BaseParser {
  def parse(filePath: String): Try[Framework]
}




