package cli

import scala.io.AnsiColor
import scala.util.matching.Regex
import flexdds.dds.DisputeState
import aspic.framework.Framework
import automatic.{DisputeStateAuto, Reasoner}
import flexdds.dds.{DisputeStateDelta, ProponentStatement, performedMovesToString}
import flexdds.enums.MoveType
import flexdds.enums.{AdvancementType, TerminationCriterion}

import scala.annotation.tailrec

object CliState {
  def apply(state: DisputeState,
            framework: Framework,
            advancement: AdvancementType,
            termination: TerminationCriterion,
            reasoner: Reasoner): CliState = {

    CliState(state, state, framework, advancement, termination, advancement.possibleMoves(framework, state), List.empty, reasoner)
  }

  private def getUserInput: String = {
    print(s"${AnsiColor.GREEN}> ")
    val input = Console.in.readLine
    print(AnsiColor.RESET)
    input
  }

  private def communicateTermination(state: CliState): Unit = {
    state.termination.checkIfOver(state.currentState, state.framework) match
      case Some(true) => println("Derivation finished. Proponent won")
      case Some(false) => println("Derivation finished. Opponent won")
      case _ =>
  }

  @tailrec
  def runCliInterface(state: CliState): Unit = {
    val obtainedState = processInput(state)
    val newState = obtainedState.copy(possibleMoves = obtainedState.advancement.possibleMoves(obtainedState.framework, obtainedState.currentState))
    communicateTermination(newState)

    if (!newState.quit) runCliInterface(newState)
  }

  @tailrec
  private def runAutomaticReasoner(state: CliState, remainingAStates: List[DisputeStateAuto]): CliState = {
    state.reasoner.run(remainingAStates)  match
      case (Some(successfulState), nRemainingAStates) => {
        val totalPerformedMoves = (state.performedMoves ++ successfulState.performedMoves)
        println(s"Successful derivation found.\n${totalPerformedMoves.performedMovesToString}")
        println(s"ENTER to accept, ; + ENTER to search for the next one.")
        getUserInput match
          case ";" => runAutomaticReasoner(state, nRemainingAStates)
          case _ => state.copy(currentState = successfulState.state, performedMoves = totalPerformedMoves)
        }
      case (None, _) =>
        println("No successful dispute derivation found.")
        state
  }

  private def processInput(state: CliState): CliState = {

    val movePattern: Regex = """^(\w+)\s+(\d+)""".r

    getUserInput match
      case "?" => state.possibleMoves.printPossibleMoves(); state
      case "q" => state.copy(quit = true)
      case "s" => println(state.currentState); state
      case "ss" => println(state.currentState.toFullString); state
      case "a" => runAutomaticReasoner(state, List(DisputeStateAuto(state.currentState)))
      case "i" => println(s"Advancement type:\t${state.advancement}\nTermination criterion:\t${state.termination}"); state
      case "b" =>
        val nextPerformedMoves = state.performedMoves.dropRight(1)
        // reconstruct the state
        val nextDState = nextPerformedMoves.foldLeft(state.initialState)((cState, move) => move.performMove(cState)(state.framework))
        state.copy(currentState = nextDState, performedMoves = nextPerformedMoves)

      case s"ca $advancementString" =>
        if (!AdvancementType.values.map(_.toString).contains(advancementString.toUpperCase)) {
          println(s"Invalid advancement type.")
          state
        } else {
          val advancement = AdvancementType.valueOf(advancementString.toUpperCase)
          println(s"Advancement type set to $advancement")
          state.copy(advancement = advancement)
        }

      case s"ct $terminationString" =>
        if (!TerminationCriterion.values.map(_.toString).contains(terminationString.toUpperCase)) {
          println(s"Invalid termination criterion..")
          state
        } else {
          val termination = TerminationCriterion.valueOf(terminationString.toUpperCase)
          println(s"Termination criterion set to $termination")
          state.copy(termination = termination)
        }

      case "m" => println(state.performedMoves.performedMovesToString); state


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
            val nextPerformedMoves = state.performedMoves :+ moveToPerform

            println(s"${nextPerformedMoves.length}: ${moveToPerform}")

            state.copy(
              currentState = nextDState, performedMoves = nextPerformedMoves)
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
                     reasoner: Reasoner,
                     quit: Boolean = false
                   ) {}


extension (possibleMoves: Map[MoveType, List[DisputeStateDelta]])
  def printPossibleMoves(): Unit = println(possibleMoves.map((moveType, moves) => s"$moveType:\n" + moves.zipWithIndex.map((move, index) => s"\t${index}: $move").mkString("\n")).mkString("\n"))
