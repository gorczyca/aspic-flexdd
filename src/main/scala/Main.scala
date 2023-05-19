import aspic.parser.AspicpParser
import cliParametersParser.{CliParser, CliParserConfig}
import flexdds.dds.{DisputeState, ProponentStatement}

import scala.util.{Failure, Success}
import aspic.framework.Framework
import cli.CliState
import automatic.{DisputeStateAuto, Reasoner}


object Main {

  def main(args: Array[String]): Unit = {

    CliParser.parse(args) match
      case Some(config) => proceed(config)
      case _ => Console.err.println("Failed to parse the CLI parameters")

  }

  private def proceed(config: CliParserConfig): Unit = {
    AspicpParser.parse(config.inputFilePath) match
      case Failure(e) => Console.err.println(s"Error while parsing the input file.\nException message: ${e.getMessage}")
      //  allow also for more goals
      case Success(framework: Framework) => 
        val goal = config.goal match
          case Some(g) => Some(g)
          case None if framework.goals.nonEmpty => Some(framework.goals.head) // TODO: allow for more goals also
          case _ => None

        goal match
          case None => Console.err.println("Need to specify the goal in CLI parameters or within the input framework file.")
          case Some(g) =>
            val state = ProponentStatement(g).performMove(DisputeState(Set(g), framework.inconsistentStrictRules,framework.inconsistentDefeasibleRules))(framework)
            val autoReasoner = Reasoner(config.advancement, config.termination, config.moveTypeOrdering, config.dfs, framework)
            val cliState = CliState(state, framework, config.advancement, config.termination, autoReasoner)
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