package flexdds.enums

import flexdds.dds.{
  DisputeState,
  DisputeStateDelta,
  ProponentRule,
  ProponentStatement,
  OpponentRule,
  OpponentStatement
}
import aspic.framework.{Framework, contraries, labelContraries}

// TODO: maybe use currying here, or something else to not repeat extracting in every move
// TODO: toList is also a repetition
enum MoveType(
    val possibleMoves: (
        DisputeState,
        Framework,
        MoveExtractor
    ) => List[DisputeStateDelta]
) {

  case PB1
      extends MoveType(possibleMoves = (state, framework, extractor) =>
        val (_, rules) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        //
        ((framework.strictRules ++ rules) -- (state.pRules ++ state.rejectedDefeasibleRules ++ state.blockedRules ++ state.pBlockedRules)).toList
          .filter(rule => state.pPlayedUnexpandedStatements.contains(rule.head))
          .map(ProponentRule.apply)
      )

  case PB2
      extends MoveType(possibleMoves = (state, framework, extractor) =>
        val (statements, rules) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        ((framework.strictRules ++ rules) -- (state.pRules ++ state.rejectedDefeasibleRules ++ state.blockedRules ++ state.pBlockedRules)).toList
          .filter(rule =>
            ((state.ordinaryPremiseCulpritsCandidates
              .contraries(framework) ++ state.defeasibleRuleCulpritsCandidates
              .labelContraries(
                framework
              ) ++ statements) -- (state.pStatements ++ state.adoptedOrdinaryPremises
              .contraries(framework) ++ state.adoptedDefeasibleRules
              .labelContraries(framework))).contains(rule.head)
          )
          .map(ProponentRule.apply)
      )

  case PF1
      extends MoveType(possibleMoves = (state, framework, extractor) =>
        val (_, rules) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        ((framework.strictRules ++ rules) -- (state.pRules ++ state.rejectedDefeasibleRules ++ state.blockedRules ++ state.pBlockedRules)).toList
          .filter(rule =>
            (!state.pStatements.contains(rule.head)
              || state.pPlayedUnexpandedStatements.contains(rule.head))
              && rule.body.subsetOf(state.pCompleteStatements)
          )
          .map(ProponentRule.apply)
      )

  case PF2
      extends MoveType(possibleMoves = (state, framework, extractor) =>
        val (statements, _) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        ((((state.ordinaryPremiseCulpritsCandidates.contraries(framework)
          ++ state.defeasibleRuleCulpritsCandidates.labelContraries(
            framework
          )) intersect framework.premises) ++ statements) -- (state.pStatements ++ framework.inconsistentStatements ++ state.rejectedOrdinaryPremises ++ state.adoptedOrdinaryPremises
          .contraries(framework) ++ state.adoptedDefeasibleRules
          .labelContraries(framework))).toList.map(ProponentStatement.apply)
      )

  case OB1
      extends MoveType(possibleMoves = (state, framework, extractor) =>
        val (statements, _) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        (framework.rules -- (state.bRules ++ state.rejectedDefeasibleRules ++ state.blockedRules))
          .filter(rule =>
            (state.bUnblockedStatementsSupportingContrariesOfAdoptedPieces union statements)
              .contains(rule.head)
          )
          .toList
          .map(OpponentRule.apply)
      )

  case OB2
      extends MoveType(possibleMoves = (state, framework, extractor) =>
        val (statements, _) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        (framework.rules -- (state.bRules ++ state.rejectedDefeasibleRules ++ state.blockedRules))
          .filter(rule =>
            (state.adoptedOrdinaryPremises
              .contraries(framework) union state.adoptedDefeasibleRules
              .labelContraries(framework) union statements).contains(rule.head)
          )
          .toList // TODO: toList
          .map(OpponentRule.apply)
      )

  case OF1
      extends MoveType(possibleMoves = (state, framework, extractor) =>
        val (_, _) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        (framework.rules -- (state.bRules ++ state.rejectedDefeasibleRules ++ state.blockedRules))
          .filter(_.body.subsetOf(state.bUnblockedCompleteStatements))
          .toList // TODO: toList
          .map(OpponentRule.apply)
      )

  case OF2
      extends MoveType(possibleMoves = (state, framework, extractor) => 
        val (statements, _) = (
          extractor.statementExtractor(state, framework),
          extractor.rulesExtractor(state, framework)
        ) // TODO: this will be repeated everywhere

        ((((state.adoptedOrdinaryPremises.contraries(framework) ++ state.adoptedDefeasibleRules.labelContraries(framework)) intersect framework.premises) ++ statements) -- (state.bStatements ++ state.rejectedOrdinaryPremises)).toList.map(OpponentStatement.apply)
      )
}
