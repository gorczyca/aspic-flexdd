package flexdds.dds

import aspic.framework.Rule

// TODO: use of reflection later prohibit!


object DisputeState {
 def apply(): DisputeState = DisputeState(Set.empty, Set.empty, Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty, Set.empty,Set.empty, Set.empty,Set.empty)
 def apply(goals: Set[String], inconsistentStrictRules: Set[Rule], inconsistentDefeasibleRules: Set[Rule]): DisputeState = DisputeState(goals, Set.empty, Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,inconsistentStrictRules,inconsistentDefeasibleRules,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty, Set.empty,Set.empty,Set.empty,Set.empty)

}

case class DisputeState(
                       // TODO: could maybe divide it into the following sub-classes
                       // basic things
                       goals: Set[String],

                       pStatements: Set[String],
                       pRules: Set[Rule],
                       oStatements: Set[String],
                       oRules: Set[Rule],

                       //
                       adoptedOrdinaryPremises: Set[String],  // defences
                       adoptedDefeasibleRules: Set[Rule],     // rule-defences
                       rejectedOrdinaryPremises: Set[String], // culprits
                       rejectedDefeasibleRules: Set[Rule],  // rule-culprits

                       //
                       blockedRules: Set[Rule],
                       pBlockedStrictRules: Set[Rule],
                       pBlockedDefeasibleRules: Set[Rule],

                       //
                       pPlayedUnexpandedStatements: Set[String],
                       playedFullyExpandedStatements: Set[String],

                       //
                       bBlockedPlayedStatements: Set[String],
                       bBlockedPlayedRules: Set[Rule],

                       //
                       pCompleteStatements: Set[String],
                       pCompleteRules: Set[Rule],

                       //
                       bUnblockedCompleteStatements: Set[String],
                       bUnblockedCompleteRules: Set[Rule],

                       // TODO: there will be one more, only for some particular semantics
                       bUnblockedStatementsSupportingContrariesOfAdoptedPieces: Set[String],
                       bUnblockedRulesSupportingContrariesOfAdoptedPieces: Set[Rule],

                       bUnblockedStatementsSupportingContrariesOfCurrentlyDefendedPieces: Set[String],
                       bUnblockedRulesSupportingContrariesOfCurrentlyDefendedPieces: Set[Rule],

                       //
                       ordinaryPremiseCulpritsCandidates: Set[String],
                       defeasibleRuleCulpritsCandidates: Set[Rule],

                       //
                       currentlyDefendedOrdinaryPremises: Set[String],
                       currentlyDefendedDefeasibleRules: Set[Rule]
                       ) {


  lazy val bRules: Set[Rule] = pRules ++ oRules
  lazy val bStatements: Set[String] = pStatements ++ oStatements
  lazy val pBlockedRules: Set[Rule] = pBlockedStrictRules ++ pBlockedDefeasibleRules //

  // TODO: only temporary
  private val minimalPropertiesNames: Seq[String] = Seq("pStatements", "pRules", "oStatements", "oRules")
  private val allPropertiesNames: Seq[String] = Seq("goals", "pStatements", "pRules", "oStatements", "oRules", "adoptedOrdinaryPremises", "adoptedDefeasibleRules", "rejectedOrdinaryPremises", "rejectedDefeasibleRules", "blockedRules", "pBlockedStrictRules", "pBlockedDefeasibleRules", "pPlayedUnexpandedStatements", "playedFullyExpandedStatements", "bBlockedPlayedStatements", "bBlockedPlayedRules", "pCompleteStatements", "pCompleteRules", "bUnblockedCompleteStatements", "bUnblockedCompleteRules", "bUnblockedStatementsSupportingContrariesOfAdoptedPieces", "bUnblockedRulesSupportingContrariesOfAdoptedPieces", "bUnblockedStatementsSupportingContrariesOfCurrentlyDefendedPieces", "bUnblockedRulesSupportingContrariesOfCurrentlyDefendedPieces", "ordinaryPremiseCulpritsCandidates", "defeasibleRuleCulpritsCandidates", "currentlyDefendedOrdinaryPremises", "currentlyDefendedDefeasibleRules")

  private def getClassFields(fieldNames: Seq[String]): String = this.getClass.getDeclaredFields.filter(f => fieldNames.contains(f.getName)).map(field => s"${field.getName}:\n\t${field.get(this).asInstanceOf[Set[Any]].mkString("; ")}").mkString("\n")

  override def toString: String = getClassFields(minimalPropertiesNames)
  def toFullString: String = getClassFields(allPropertiesNames)


  @deprecated
  def toStringDecorate: String = {


    val decorate: Set[String] | Set[Rule] => String => String => String = elems => decor => delimit => elems.map(s => s"$decor$s").mkString(delimit)

    val z = decorate(pStatements)("*")("; ")

    ???

  }
}
