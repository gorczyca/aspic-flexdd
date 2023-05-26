import aspic.parser.BaseParser
import cliParametersParser.{CliParser, CliParserConfig}
import flexdds.dds.{DisputeState, ProponentStatement}

import logger.log

import scala.util.{Failure, Success}
import aspic.framework.Framework
import cli.CliState
import automatic.{DisputeStateAuto, Reasoner}
import flexdds.enums.MoveType


object Main {

  def main(args: Array[String]): Unit = {

    CliParser.parse(args) match
      case Some(config) => proceed(config)
      case _ => Console.err.println("Failed to parse the CLI parameters")

  }



  private def proceed(config: CliParserConfig): Unit = {

    BaseParser.get(config.inputFormat) match
      case None => Console.err.println(s"Wrong input format.")
      case Some(parser) => parser.parse(config.inputFilePath) match
        case Failure(e) => Console.err.println(s"Error while parsing the input file.\nException message: ${e.getMessage}")
        //  allow also for more goals
        case Success(framework: Framework) =>

          log(config.log)("Parsed framework")

          val goal = config.goal match
            case Some(g) => Some(g)
            case None if framework.goals.nonEmpty => Some(framework.goals.head) // TODO: allow for more goals also
            case _ => None

          goal match
            case None => Console.err.println("Need to specify the goal in CLI parameters or within the input framework file.")
            case Some(g) =>

              log(config.log)("Checking inconsistent goals")

              if (framework.inconsistentStatements.contains(g)) { println("NO"); return }

              log(config.log)("Creating initial state")

              val state = ProponentStatement(g).performMove(DisputeState(Set(g), framework.inconsistentStrictRules,framework.inconsistentDefeasibleRules))(framework)

              log(config.log)("Creating auto reasoner")

              val autoReasoner = Reasoner(config.advancement, config.termination, config.moveTypeOrdering, config.dfs, framework)

              log(config.log)("Creating CLI state")

              val cliState = CliState(state, framework, config.advancement, config.termination, autoReasoner, logger = log(config.log))
              if (config.solve) {

                autoReasoner.run(List(DisputeStateAuto(state))) match
                  case (Some(_), _) => println("YES")
                  case _ => println("NO")

              } else {
                println("Derivation started.")

                CliState.runCliInterface(cliState)
              }
  }
}