package automatic

import aspic.framework.{Framework, contraries, contrariesOf, statementContraries, labels}
import flexdds.dds.{DisputeStateDelta, ProponentRule}
import flexdds.enums.{AdvancementType, MoveType, TerminationCriterion, filterPossibleMoves}
import flexdds.enums.MoveType.{PB1, PB2, PF1, PF2, OB1, OB2, OF1, OF2}
import flexdds.dds.{RuleMove, StatementMove}


import scala.annotation.tailrec

case class Reasoner(advancement: AdvancementType,
                    termination: TerminationCriterion,
                    moveTypeOrdering: Seq[MoveType],
                    dfs: Boolean,
                    framework: Framework) {
  override def toString: String = {
    s"Move type ordering:\t\t[${moveTypeOrdering.mkString(", ")}]\n" +
      s"Search type:\t\t${if (dfs) "DFS" else "BFS"}"
  }


  /**
   * Filters out the moves which cannot be performed due to the previous choices from the currentState, e.g. attacking ignored culprit pieces or adopting ignored defeasible pieces
   *
   * @param possibleMoves
   * @param currentState
   * @return
   */
//  val filterMoves: (DisputeStateAuto, Framework) => DisputeStateDelta => Boolean = (state, framework) => move => {
//    move.statements.intersect(state.ignoredOrdinaryPremises).isEmpty &&
//      move.statements.contrariesOf(framework).intersect(state.ignoredOrdinaryPremiseCulpritCandidates ++ state.ignoredDefeasibleRuleCulpritCandidates.labels).isEmpty && (move match {
//      case ProponentRule(rule) => !state.ignoredDefeasibleRules.contains(rule)
//      case _ => true
//    })
//  }
  val filterMoves: (DisputeStateAuto, Framework) => DisputeStateDelta => Boolean = (state, framework) => move => {
    move.statements.intersect(state.ignoredOrdinaryPremises).isEmpty &&
      move.statements.contrariesOf(framework).intersect(state.ignoredNotAttackedStatements).isEmpty
//      && (move match {
//      case ProponentRule(rule) => !state.ignoredDefeasibleRules.contains(rule)
//      case _ => true
//    })
  }


  private def generateNewDisputeStates(currentAState: DisputeStateAuto): List[DisputeStateAuto] = {

    val currentState = currentAState.state

    val possibleMoves = advancement.possibleMoves(framework, currentState).filterPossibleMoves(filterMoves(currentAState, framework))
    if (possibleMoves.isEmpty)
      return List.empty

    val chosenMoveType = possibleMoves.keys.map(mType => (mType, moveTypeOrdering.indexOf(mType))).minBy(_._2)._1

    // all possible moves of type chosenMoveType
    val movesToChoose = possibleMoves(chosenMoveType)
    // moves get grouped into chunks, depending on their type:
      // if they are rule moves, then they are grouped by their heads, and the smallest chunk is chosen
      // otherwise (if assumption moves) single assumption is returned
    val (statement, movesChunk): (String, List[DisputeStateDelta]) = movesToChoose.groupBy(_.premiseOrRuleHead).minBy { case (_, moves) => moves.length}
//    val (statement, movesChunk): (String, List[DisputeStateDelta]) = if (MoveType.ruleMoves.contains(chosenMoveType)) {
//        movesToChoose.groupBy { case move:RuleMove => move.rule.head }.minBy { case (_, moves) => moves.length }
//      } else {
//        movesToChoose.groupBy { case move:StatementMove => move.statement  }
//      }


    // auxiliary things
    // statements attacked by the "statement" (either a head of rules or a premise)
    val attackedByStatement = Set(statement).contrariesOf(framework)

    // List[(DisputeStateDelta, DisputeState)] containing tuples: (move to be performed, state after performing it)
    val performedMovesChunk = movesChunk.map(move => (move, move.performMove(currentState)(framework)))
    // List[DisputeStateAuto] list containing all DisputeStateAuto after performing moves from performed moves chunk
    val newAStates = performedMovesChunk.map{ case (move, newState) => currentAState.copy(state = newState, performedMoves = currentAState.performedMoves :+ (move, chosenMoveType)) }

    // depending on the move type chosen decide how to prepare the next moves
    chosenMoveType match
      case PB1 =>
        // perform all moves in each state
        newAStates
      case PB2 =>
        // perform all moves in each state, also add a move ignoring whatever head (statement) is attacking
        newAStates :+ currentAState.copy(ignoredNotAttackedStatements = currentAState.ignoredNotAttackedStatements ++ attackedByStatement)
      case PF2 =>
        // use the assumption or not
        newAStates.take(1) :+ currentAState.copy(ignoredOrdinaryPremises = currentAState.ignoredOrdinaryPremises ++ Set(statement))
      case _ =>
        // in every all opponent moves and PF1 there's non-determinism, just perform a move
        newAStates.take(1)

  }

  /**
   *
   * @param remainingAStates data structure containing remaining states, stack if DFS and queue if BFS
   * @return
   */
  @tailrec
  final def run(remainingAStates: List[DisputeStateAuto]): (Option[DisputeStateAuto], List[DisputeStateAuto]) = { // 1. found successful dispute state auto, 2. remaining states to check if the next one is to be



    if (remainingAStates.isEmpty)
      return (None, remainingAStates)

    val currentAState :: nRemainingAStates = remainingAStates // pop the first from the remaining states
    val currentState = currentAState.state

    termination.checkIfOver(currentState, framework, filterMoves(currentAState, framework)) match
      case Some(true) => (Some(currentAState), nRemainingAStates)
      case Some(false) => run(nRemainingAStates)
      case _ =>
        val newAStates = generateNewDisputeStates(currentAState)
        val nextDisputeStates = if (dfs) newAStates ++ nRemainingAStates else nRemainingAStates ++ newAStates
        run(nextDisputeStates)
  }
}
