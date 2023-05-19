package flexdds.enums

import flexdds.dds.DisputeState
import aspic.framework.{Rule, Framework, contraries, statementContraries}
import flexdds.enums.MoveType.{PB1, PB2, PF1, PF2, OB1, OB2, OF1, OF2}
import flexdds.enums.MoveExtractor.{RuleExtractor, StatementExtractor}
import flexdds.dds.DisputeStateDelta

enum AdvancementType(val allowedMoves: Map[Seq[MoveType], MoveExtractor]) {

  case DAB
      extends AdvancementType(allowedMoves =
        Map(
          // PB1, PB2 move
          Seq(PB1, PB2) -> RuleExtractor((_, framework) =>
            framework.defeasibleRules
          ),
          Seq(PF2, OB1, OB2, OF2) -> MoveExtractor()
        )
      )

  // ----
  case DABF
      extends AdvancementType(allowedMoves =
        Map(
          // PB1, PB2 move
          Seq(PB1, PB2) -> RuleExtractor((_, framework) =>
            framework.defeasibleRules
          ),
          Seq(PF1, PF2, OB1, OB2, OF2) -> MoveExtractor()
        )
      )

  // ---
  case DC
      extends AdvancementType(allowedMoves =
        Map(
          // PB1, PB2 move
          Seq(PB1, PB2) -> RuleExtractor((_, framework) =>
            framework.defeasibleRules
          ),

          // PF1, PF2
          Seq(PF1, PF2) -> MoveExtractor(
            statementExtractor =
              (state, _) => state.currentlyDefendedOrdinaryPremises,
            rulesExtractor =
              (state, _) => state.currentlyDefendedDefeasibleRules
          ),

          // Caution: here different for OB1 and OB2, even though same parameters in the description, because in OB1 used further as a parameter
          // OB1, OB2
          Seq(OB1) -> StatementExtractor((state, _) =>
            state.bUnblockedStatementsSupportingContrariesOfCurrentlyDefendedPieces
          ),
          Seq(OB2, OF2) -> StatementExtractor((state, framework) =>
            state.currentlyDefendedOrdinaryPremises
              .contraries(framework) ++ state.currentlyDefendedDefeasibleRules
              .statementContraries(framework)
          )
        )
      )

  case DS
      extends AdvancementType(allowedMoves =
        Map(
          Seq(PB1, PB2) -> RuleExtractor((_, framework) =>
            framework.defeasibleRules
          ),
          Seq(PF1, PF2) -> MoveExtractor(
            statementExtractor =
              (_, framework) => framework.premises, // TODO here
            rulesExtractor =
              (_, framework) => framework.defeasibleRules // TODO: here
          ),
          Seq(OB1, OB2, OF2) -> MoveExtractor()
        )
      )

  def possibleMoves(framework: Framework, state: DisputeState): Map[MoveType, List[DisputeStateDelta]] = {

    val allowedMovesFlattened = this.allowedMoves.flatMap((moveTypeSeq, extr) => moveTypeSeq.map(moveType => (moveType, extr)))

    allowedMovesFlattened.map((moveType, extractor) => (moveType, moveType.possibleMoves(state, framework, extractor.extract(state, framework)).toList)).filterEmptyOut
  }    
}

extension (possibleMoves: Map[MoveType, List[DisputeStateDelta]])

  def filterPossibleMoves(filterMove: DisputeStateDelta => Boolean): Map[MoveType, List[DisputeStateDelta]] = possibleMoves.map((moveType, moves) => (moveType, moves.filter(filterMove))).filterEmptyOut

  def filterEmptyOut: Map[MoveType, List[DisputeStateDelta]] = possibleMoves.filter(_._2.nonEmpty)

