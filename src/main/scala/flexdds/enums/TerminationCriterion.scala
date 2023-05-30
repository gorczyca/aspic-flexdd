package flexdds.enums

import aspic.framework.{Framework, contraries, statementContraries}
import flexdds.dds.{DisputeState, DisputeStateDelta}
import flexdds.enums.MoveType.{OB1, OB2, OF1, OF2, PB1, PB2, PF1, PF2}
import flexdds.enums.filterPossibleMoves
import flexdds.enums.AdvancementType.{DAB, DABF, DC, DS}


import scala.collection.immutable.Set

case class AdvancementMoves(advancement: AdvancementType, moves: Set[MoveType])


enum TerminationCriterion(proponentWonMoves: Set[AdvancementMoves], opponentWonMoves: Set[AdvancementMoves]) {

  def checkIfOver(state: DisputeState, framework: Framework, filterMoves: DisputeStateDelta => Boolean = _ => true): Option[Boolean] = {
    if (proponentWinning(state, framework)) {

      // debugging

      // TODO: remove
      val unfilteredMoves =  this.proponentWonMoves.map(advMoves => (advMoves.advancement, advMoves.advancement.possibleMoves(framework, state).filter(x => advMoves.moves.contains(x._1))))

      val filteredMoves =  this.proponentWonMoves.map(advMoves => (advMoves.advancement, advMoves.advancement.possibleMoves(framework, state).filterPossibleMoves(filterMoves).filter(x => advMoves.moves.contains(x._1))))


      if (this.proponentWonMoves.forall(advMoves => (advMoves.advancement.possibleMoves(framework, state).filterPossibleMoves(filterMoves).keySet intersect advMoves.moves).isEmpty)) Some(true)
      else None
    } else {

    // TODO: remove
//      val unfilteredMoves = this.opponentWonMoves.map(advMoves => (advMoves.advancement, advMoves.advancement.possibleMoves(framework, state).filter(x => advMoves.moves.contains(x._1))))
//
//      val filteredMoves = this.opponentWonMoves.map(advMoves => (advMoves.advancement, advMoves.advancement.possibleMoves(framework, state).filterPossibleMoves(filterMoves).filter(x => advMoves.moves.contains(x._1))))
//

      if (this.opponentWonMoves.forall(advMoves => (advMoves.advancement.possibleMoves(framework, state).filterPossibleMoves(filterMoves).keySet intersect advMoves.moves).isEmpty)) Some(false)
      else None
    }
  }

  private def proponentWinning(state: DisputeState, framework: Framework): Boolean =
//    (state.goals ++ state.rejectedOrdinaryPremises.contraries(framework) ++ state.rejectedDefeasibleRules.statementContraries(framework)).subsetOf(state.pCompleteStatements) && (state.adoptedOrdinaryPremises.contraries(framework) ++ state.adoptedDefeasibleRules.statementContraries(framework)).intersect(state.bUnblockedCompleteStatements).isEmpty
    (state.goals.subsetOf(state.pCompleteStatements) &&  state.rejectedOrdinaryPremises.forall(Set(_).contraries(framework).intersect(state.pCompleteStatements).nonEmpty) && state.rejectedDefeasibleRules.forall(Set(_).statementContraries(framework).intersect(state.pCompleteStatements).nonEmpty) && (state.adoptedOrdinaryPremises.contraries(framework) ++ state.adoptedDefeasibleRules.statementContraries(framework)).intersect(state.bUnblockedCompleteStatements).isEmpty)

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