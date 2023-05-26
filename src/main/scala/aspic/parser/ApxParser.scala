package aspic.parser

import aspic.framework.{Contrary, Rule}

object ApxParser extends RegexParser {

  override def strictRule: Parser[Rule] = "rule("~identifier~",["~fields~"])." ^^ { case "rule("~head~",["~body~"])." => new Rule(head, body)  }

  override def contrary: Parser[Contrary] = "contrary("~identifier~","~identifier~")." ^^ { case "contrary("~ass~","~ctr~")." => Contrary(ass, ctr) }


  override def ordinaryPremise: Parser[String] = "asm("~identifier~")." ^^ { case "asm("~ass~")." => ass }


  override def goal: Parser[String] = "goal("~identifier~")." ^^ { case "goal("~goal~")." => goal }


  private def neverMatch[T]: Parser[T] = (in: Input) => Failure("Never match", in)
  override def defeasibleRule: ApxParser.Parser[Rule] = neverMatch[Rule]
  override def axiom: ApxParser.Parser[String] = neverMatch[String]

}
