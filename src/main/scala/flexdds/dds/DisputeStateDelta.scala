package flexdds.dds

import aspic.framework.{Framework, Rule, contraries, contrariesOf, ruleContrariesOf}

sealed trait DisputeStateDelta {
  def performMove(state: DisputeState)(implicit framework: Framework): DisputeState = {
    val (pStatements, pRules, oStatements, oRules) = this match
      case ProponentStatement(s) => (Set(s), Set.empty[Rule], Set.empty, Set.empty[Rule])
      case ProponentRule(r) => (r.statements, Set(r), Set.empty[Rule], Set.empty[Rule])
      case OpponentStatement(s) => (Set.empty[String], Set.empty[Rule], Set(s), Set.empty[Rule])
      case OpponentRule(r) => (Set.empty[String], Set.empty[Rule], r.statements, Set(r))

      //
      val nPStatements = state.pStatements ++ pStatements
      val nPRules = state.pRules ++ pRules
      val nOStatements = state.oStatements ++ oStatements
      val nORules = state.oRules ++ oRules

      //
      val newAdoptedOrdinaryPremises = pStatements intersect framework.ordinaryPremises
      val nAdoptedOrdinaryPremises = state.adoptedOrdinaryPremises ++ newAdoptedOrdinaryPremises
      val newAdoptedDefeasibleRules = pRules intersect framework.defeasibleRules
      val nAdoptedDefeasibleRules = state.adoptedDefeasibleRules ++ newAdoptedOrdinaryPremises
      val newRejectedOrdinaryPremises = pStatements.contrariesOf intersect framework.ordinaryPremises
      val nRejectedOrdinaryPremises = state.rejectedOrdinaryPremises ++ newRejectedOrdinaryPremises
      val nRejectedDefeasibleRules = state.rejectedDefeasibleRules ++ pStatements.ruleContrariesOf

      //
      val nBlockedRules = state.blockedRules ++ framework.rules.filter(rule => (rule.body intersect newRejectedOrdinaryPremises).nonEmpty)
      val nPBlockedStrictRules = state.pBlockedStrictRules ++ framework.strictRules.filter(rule => (rule.statements intersect (newAdoptedOrdinaryPremises union newAdoptedDefeasibleRules.map(_.label.get)).contraries).nonEmpty)

      val x = 1
      DisputeState()

  }
}

case class ProponentStatement(statement: String) extends DisputeStateDelta
case class ProponentRule(rule: Rule) extends DisputeStateDelta
case class OpponentStatement(statement: String) extends DisputeStateDelta
case class OpponentRule(rule: Rule) extends DisputeStateDelta


