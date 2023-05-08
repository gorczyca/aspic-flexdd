package aspic.parser

import aspic.framework.{Contrary, Framework, Rule}

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Using}


abstract class Parser extends RegexParsers {

  protected def identifier: Parser[String] = """[a-zA-Z_$0-9]+""".r
  protected def fields: Parser[Set[String]] = (identifier~",".?).* ^^ { _.map(_._1).toSet } // create a sorted set out of a set

  def defeasibleRule: Parser[Rule]
  def strictRule: Parser[Rule]
  def contrary: Parser[Contrary]

  def axiom: Parser[String]
  def ordinaryPremise: Parser[String]
  def goal: Parser[String]

  private def parseFileLines(implicit fileLines: List[String]): Framework = {
    Framework(
      strictRules = parseLines(strictRule),
      defeasibleRules = parseLines(defeasibleRule),
      contraries = parseLines(contrary),
      axioms = parseLines(axiom),
      ordinaryPremises = parseLines(ordinaryPremise),
      goals = parseLines(goal),
    )
  }

  private def parseLines[A](parser: Parser[A])(implicit lines: List[String]): Set[A] = {
    lines.map(parseAll[A](parser, _)).filter {
      case Success(_, _) => true
      case _ => false
    }.map(_.get).toSet
  }

  def parse(filePath: String): Try[Framework] = {
    Using(Source.fromFile(filePath, enc = "UTF-8")) {
      source => {

        val sourceLines = source.getLines().toList
        parseFileLines(sourceLines)

      }
    }
  }

}


