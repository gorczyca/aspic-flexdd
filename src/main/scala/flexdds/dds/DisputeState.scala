package flexdds.dds

import aspic.framework.Rule

object DisputeState {
  // TODO: dumb
 def apply(): DisputeState = DisputeState(Set.empty, Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty, Set.empty)
 def apply(inconsistentRules: Set[Rule]): DisputeState = DisputeState(Set.empty, Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,inconsistentRules,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty, Set.empty)

}

case class DisputeState(
                       // TODO: could maybe divide it into the following sub-classes
                       // basic things
                       pStatements: Set[String],
                       pRules: Set[Rule],
                       oStatements: Set[String],
                       oRules: Set[Rule],

                       //
                       adoptedOrdinaryPremises: Set[String],  // defences
                       adoptedDefeasibleRules: Set[Rule],     // rule-defences
                       rejectedOrdinaryPremises: Set[String], // culprits
                       rejectedDefeasibleRules: Set[String],  // rule-culprits

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
                       bUnblockedSSupportingStatements: Set[String],
                       bUnblockedSSupportingRules: Set[Rule],

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



  override def toString: String = {


    val decorate: Set[String] | Set[Rule] => String => String => String = elems => decor => delimit => elems.map(s => s"$decor$s").mkString(delimit)

    val z = decorate(pStatements)("*")("; ")

    ???

  }
}
