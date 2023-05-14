package flexdds.enums

import flexdds.dds.DisputeState
import aspic.framework.{Rule, Framework, contraries, labelContraries}
import flexdds.enums.MoveType.{PB1, PB2, PF1, PF2, OB1, OB2, OF1, OF2}
import flexdds.enums.MoveExtractor.{RuleExtractor, StatementExtractor}

enum AdvancementType(val allowedMoves: Map[Set[MoveType], MoveExtractor]) {

  case DAB
      extends AdvancementType(allowedMoves =
        Map(
          // PB1, PB2 move
          Set(PB1, PB2) -> RuleExtractor((_, framework) => framework.defeasibleRules),

          Set(PF2, OB1, OB2, OF2) -> MoveExtractor(),
        )
      )

  // ----
  case DABF
      extends AdvancementType(allowedMoves =
        Map(
          // PB1, PB2 move
          Set(PB1, PB2) -> RuleExtractor((_, framework) => framework.defeasibleRules),

          Set(PF1, PF2, OB1, OB2, OF2) -> MoveExtractor(),
        )
      )

  // ---
  case DC
      extends AdvancementType(allowedMoves =
        Map(
          // PB1, PB2 move
          Set(PB1, PB2) -> RuleExtractor((_, framework) => framework.defeasibleRules),

          // PF1, PF2
          Set(PF1, PF2) -> MoveExtractor(
            statementExtractor = (state, _) => state.currentlyDefendedOrdinaryPremises, 
            rulesExtractor = (state, _) => state.currentlyDefendedDefeasibleRules 
          ),

          // Caution: here different for OB1 and OB2, even though same parameters in the description, because in OB1 used further as a parameter
          // OB1, OB2
          Set(OB1) -> StatementExtractor((state, _) => state.bUnblockedStatementsSupportingContrariesOfCurrentlyDefendedPieces),
          
          Set(OB2, OF2) -> StatementExtractor((state, framework) => state.currentlyDefendedOrdinaryPremises.contraries(framework) ++ state.currentlyDefendedDefeasibleRules.labelContraries(framework)),
        )
      )

  case DS
      extends AdvancementType(allowedMoves =
        Map(
          Set(PB1, PB2) -> RuleExtractor((_, framework) => framework.defeasibleRules),
          Set(PF1, PF2) -> MoveExtractor(
            statementExtractor = (_, framework) => framework.premises, // TODO here
            rulesExtractor = (_, framework) => framework.defeasibleRules // TODO: here
          ),

          Set(OB1, OB2, OF2) -> MoveExtractor(),
        )
      )

// TODO:  DF
}
