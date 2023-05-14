import aspic.parser.AspicpParser
import cliParametersParser.{CliParser, CliParserConfig}
import flexdds.dds.{DisputeState, ProponentStatement}

import scala.util.{Failure, Success}
import aspic.framework.Framework



object Main {

  def main(args: Array[String]): Unit = {

    CliParser.parse(args) match
      case Some(config) => proceed(config)
      case _ => Console.err.println("Failed to parse the CLI parameters")

  }

  private def proceed(config: CliParserConfig): Unit = {
    AspicpParser.parse(config.inputFilePath) match
      case Success(framework: Framework) =>
        // TODO: don't do it here
        //  allow also for more goals
        config.goal match
          case None => Console.err.println("Need to specify the goal in CLI parameters.")
          case Some(goal) =>
            val initialDState = ProponentStatement(goal).performMove(DisputeState(framework.inconsistentStrictRules, framework.inconsistentDefeasibleRules))(framework)
            val x = 1

      case _ => Console.err.println("Error while parsing the input file.")
  }
}