package flexdds.enums

import aspic.framework.{Framework, Rule}
import flexdds.dds.DisputeState

case class AdditionalPieces(statements: Set[String], rules: Set[Rule])


type Extractor[A] = (DisputeState, Framework) => Set[A]

object MoveExtractor {

  def apply(): MoveExtractor = MoveExtractor((_, _) => Set.empty[String], (_, _) => Set.empty[Rule])

  def RuleExtractor(ruleExtractor: (DisputeState, Framework) => Set[Rule]): MoveExtractor = MoveExtractor((_, _) => Set.empty[String], ruleExtractor)

  def StatementExtractor(statementExtractor: Extractor[String]): MoveExtractor = MoveExtractor(statementExtractor, (_, _) => Set.empty[Rule])
}

case class MoveExtractor(statementExtractor: Extractor[String], rulesExtractor: Extractor[Rule]) {
  def extract(state: DisputeState, framework: Framework): AdditionalPieces = AdditionalPieces(statementExtractor(state, framework), rulesExtractor(state, framework))
}
