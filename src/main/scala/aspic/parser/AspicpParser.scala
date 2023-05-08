package aspic.parser
import aspic.framework.{Contrary, Rule}

object AspicpParser extends Parser {
  override def defeasibleRule: Parser[Rule] = identifier ~ ":" ~ identifier ~ ":-" ~ fields ~ "." ^^ {
    case label ~ ":" ~ head ~ ":-" ~ body ~ "." => Rule(head, body, isStrict = false, Some(label))
  }

  override def strictRule: Parser[Rule] = identifier ~ ":-" ~ fields ~ "." ^^ {
    case head ~ ":-" ~ body ~ "." => new Rule(head, body)
  }

  override def contrary: Parser[Contrary] = identifier ~ ":" ~ identifier ~ "." ^^ { case statement ~ ":" ~ statementContrary ~ "." => Contrary(statement, statementContrary) }

  override def axiom: Parser[String] = identifier ~ "." ^^ { case axiom ~ "." => axiom  }

  override def ordinaryPremise: Parser[String] = "^" ~ identifier ~ "." ^^ { case "^" ~ ordinaryPremise ~ "." =>  ordinaryPremise }

  override def goal: Parser[String] = "?" ~ identifier ~ "." ^^ { case "?" ~ goal ~ "." => goal }
}
