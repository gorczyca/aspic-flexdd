package flexdds.enums

import flexdds.dds.{DisputeState, DisputeStateDelta, OpponentRule, OpponentStatement, ProponentRule, ProponentStatement}
import aspic.framework.{Framework, Rule, contraries, statementContraries}

import scala.collection.immutable.Set


// TODO: maybe use currying here, or something else to not repeat extracting in every move
// TODO: toList is also a repetition
enum MoveType(
    val possibleMoves: (
        DisputeState,
        Framework,
        AdditionalPieces
    ) => Set[DisputeStateDelta]
) {




  case PB1
      extends MoveType(possibleMoves = (state, framework, pieces) =>

        //
        ((framework.strictRules ++ pieces.rules) -- (state.pRules ++ state.rejectedDefeasibleRules ++ state.blockedRules ++ state.pBlockedRules))
          .filter(rule => state.pPlayedUnexpandedStatements.contains(rule.head))
          .map(ProponentRule.apply)
      )

  case PB2
      extends MoveType(possibleMoves = (state, framework, pieces) =>

        ((framework.strictRules ++ pieces.rules) -- (state.pRules ++ state.rejectedDefeasibleRules ++ state.blockedRules ++ state.pBlockedRules))
          .filter(rule =>
            ((state.ordinaryPremiseCulpritsCandidates
              .contraries(framework) ++ state.defeasibleRuleCulpritsCandidates
              .statementContraries(
                framework
              ) ++ pieces.statements) -- (state.pStatements ++ state.adoptedOrdinaryPremises
              .contraries(framework) ++ state.adoptedDefeasibleRules
              .statementContraries(framework))).contains(rule.head)
          )
          .map(ProponentRule.apply)
      )

  case PF1
      extends MoveType(possibleMoves = (state, framework, pieces) =>

        ((framework.strictRules ++ pieces.rules) -- (state.pRules ++ state.rejectedDefeasibleRules ++ state.blockedRules ++ state.pBlockedRules))
          .filter(rule =>
            (!state.pStatements.contains(rule.head)
              || state.pPlayedUnexpandedStatements.contains(rule.head))
              && rule.body.subsetOf(state.pCompleteStatements)
          )
          .map(ProponentRule.apply)
      )

  case PF2
      extends MoveType(possibleMoves = (state, framework, pieces) =>

        ((((state.ordinaryPremiseCulpritsCandidates.contraries(framework)
          ++ state.defeasibleRuleCulpritsCandidates.statementContraries(
            framework
          )) intersect framework.premises) ++ pieces.statements) -- (state.pStatements ++ framework.inconsistentStatements ++ state.rejectedOrdinaryPremises ++ state.adoptedOrdinaryPremises
          .contraries(framework) ++ state.adoptedDefeasibleRules
          .statementContraries(framework))).map(ProponentStatement.apply)
      )

  case OB1
      extends MoveType(possibleMoves = (state, framework, pieces) =>

        (framework.rules -- (state.bRules ++ state.rejectedDefeasibleRules ++ state.blockedRules))
          .filter(rule =>
            (state.bUnblockedStatementsSupportingContrariesOfAdoptedPieces union pieces.statements)
              .contains(rule.head)
          )
          .map(OpponentRule.apply)
      )

  case OB2
      extends MoveType(possibleMoves = (state, framework, pieces) =>

        (framework.rules -- (state.bRules ++ state.rejectedDefeasibleRules ++ state.blockedRules))
          .filter(rule =>
            (state.adoptedOrdinaryPremises
              .contraries(framework) union state.adoptedDefeasibleRules
              .statementContraries(framework) union pieces.statements).contains(rule.head)
          )
          .map(OpponentRule.apply)
      )

  case OF1
      extends MoveType(possibleMoves = (state, framework, _) =>

        (framework.rules -- (state.bRules ++ state.rejectedDefeasibleRules ++ state.blockedRules))
          .filter(_.body.subsetOf(state.bUnblockedCompleteStatements))
          .map(OpponentRule.apply)
      )

  case OF2
      extends MoveType(possibleMoves = (state, framework, pieces) => 

        ((((state.adoptedOrdinaryPremises.contraries(framework) ++ state.adoptedDefeasibleRules.statementContraries(framework)) intersect framework.premises) ++ pieces.statements) -- (state.bStatements ++ state.rejectedOrdinaryPremises)).map(OpponentStatement.apply)
      )
}

