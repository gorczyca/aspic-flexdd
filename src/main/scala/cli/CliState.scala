package cli

import scala.io.AnsiColor
import scala.util.matching.Regex

import flexdds.dds.DisputeState
import aspic.framework.Framework
import flexdds.dds.{DisputeStateDelta, ProponentStatement}
import flexdds.enums.MoveType
import flexdds.enums.{TerminationCriterion, AdvancementType}

object CliState {
  def apply(goal: String,
            framework: Framework,
            advancement: AdvancementType,
            termination: TerminationCriterion): CliState = {
    val state = ProponentStatement(goal).performMove(DisputeState(framework.inconsistentStrictRules,framework.inconsistentDefeasibleRules))(framework)

    CliState(state, state, framework, advancement, termination, advancement.possibleMoves(framework, state), List.empty)
  }

  private def getUserInput: String = {
    print(s"${AnsiColor.GREEN}> ")
    val input = Console.in.readLine
    print(AnsiColor.RESET)
    input
  }

  def runCliInterface(state: CliState): Unit = {
    val newState = processInput(state)

    if (!newState.quit) runCliInterface(newState)
  }

  private def processInput(state: CliState): CliState = {

    val movePattern: Regex = """^(\w+)\s+(\d+)""".r

    getUserInput match
      case "?" => state.possibleMoves.printPossibleMoves(); state
      case "q" => state.copy(quit = true)
      case "s" => println(state.currentState); state
      case "ss" => println(state.currentState.toFullString); state

      case movePattern(moveString, indexString) if MoveType.values.map(_.toString).contains(moveString.toUpperCase) =>
        val move = MoveType.valueOf(moveString.toUpperCase)
        val index = indexString.toInt
        if (!state.possibleMoves.contains(move)) {
          println(s"Move $move not applicable.");
          state
        }
        else {
          if (state.possibleMoves(move).length < index) {
            println(s"Type $move has no move of index $index applicable.");
            state
          }
          else {
            val moveToPerform = state.possibleMoves(move)(index)
            val nextDState = moveToPerform.performMove(state.currentState)(state.framework)
            val nextPossibleMoves = state.advancement.possibleMoves(state.framework, nextDState)
            val nextPerformedMoves = state.performedMoves :+ moveToPerform

            println(s"${nextPerformedMoves.length}: ${moveToPerform}")

            state.copy(
              currentState = nextDState, possibleMoves = nextPossibleMoves, performedMoves = nextPerformedMoves)
          }
        }

      case _ => System.err.println("Wrong input."); state
  }

}

case class CliState(
                     initialState: DisputeState,
                     currentState: DisputeState,
                     framework: Framework,
                     advancement: AdvancementType,
                     termination: TerminationCriterion,
                     possibleMoves: Map[MoveType, List[DisputeStateDelta]],
                     performedMoves: List[DisputeStateDelta],
                     quit: Boolean = false
                   ) {}


extension (possibleMoves: Map[MoveType, List[DisputeStateDelta]])
  def printPossibleMoves(): Unit = println(possibleMoves.map((moveType, moves) => s"$moveType:\n" + moves.zipWithIndex.map((move, index) => s"\t${index}: $move").mkString("\n")).mkString("\n"))
