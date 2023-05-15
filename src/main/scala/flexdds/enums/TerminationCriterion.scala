package flexdds.enums

import aspic.framework.{Framework, contraries, labelContraries}
import flexdds.dds.DisputeState
import flexdds.enums.MoveType.{OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import flexdds.enums.AdvancementType.{DAB, DABF, DC, DS}

import java.awt.Frame
import scala.collection.immutable.Set

case class AdvancementMoves(advancement: AdvancementType, moves: Set[MoveType])


enum TerminationCriterion(proponentWonMoves: Set[AdvancementMoves], opponentWonMoves: Set[AdvancementMoves]) {

  def checkIfOver(state: DisputeState, framework: Framework): Option[Boolean] = {
    if (proponentWinning(state, framework)) {
      if (this.proponentWonMoves.forall(advMoves => (advMoves.advancement.possibleMoves(framework, state).keySet intersect advMoves.moves).isEmpty)) Some(true)
      else None
    } else {
      if (this.opponentWonMoves.forall(advMoves => (advMoves.advancement.possibleMoves(framework, state).keySet intersect advMoves.moves).isEmpty)) Some(false)
      else None
    }
  }

  private def proponentWinning(state: DisputeState, framework: Framework): Boolean =
    (state.goals ++ state.rejectedOrdinaryPremises.contraries(framework) ++ state.rejectedDefeasibleRules.labelContraries(framework)).subsetOf(state.pCompleteStatements) && (state.adoptedOrdinaryPremises.contraries(framework) ++ state.adoptedDefeasibleRules.labelContraries(framework)).intersect(state.bUnblockedCompleteStatements).isEmpty

  case TA extends TerminationCriterion(
    proponentWonMoves = Set(AdvancementMoves(DAB, Set(OB1, OB2, OF2))),
    opponentWonMoves = Set(AdvancementMoves(DAB, Set(PB1, PB2, PF2))))

  case TC extends TerminationCriterion(
    proponentWonMoves = Set(AdvancementMoves(DC, Set(PF1, PF2)), AdvancementMoves(DAB, Set(OB1, OB2, OF2))),
    opponentWonMoves = Set(AdvancementMoves(DC, Set(PB1, PB2, PF1, PF2)))
  )

  case TS extends TerminationCriterion(
    proponentWonMoves = Set(AdvancementMoves(DS, Set(PF1, PF2)), AdvancementMoves(DAB, Set(OB1, OB2, OF2))),
    opponentWonMoves = Set(AdvancementMoves(DS, Set(PB1, PB2, PF1, PF2)))
  )
}