package cliParametersParser

import flexdds.enums.{AdvancementType, MoveType, TerminationCriterion}
import scopt.OParser
object CliParser {

  private val builder = OParser.builder[CliParserConfig]
  private val parser = {
    import builder._
    OParser.sequence(
      programName("[Program name]"),
      head("aspic-flexdds", "0,1"),
      arg[String]("<input file>")
        .required()
        .action((x, c) => c.copy(inputFilePath = x)),
      opt[String]('i', "inputFormat")
        .action((x, c) => c.copy(inputFormat = x))
        .validate {
          case "aspicp" => success
          case _ => failure("Input format must be one of the following:\n aspicp")
        }
        .text("Input format.")
        .valueName("<input format>"),
      opt[String]('g', "goal")
        .action((x, c) => c.copy(goal = Some(x))),
      opt[Unit]('s', "solve")
        .action((_, c) => c.copy(solve = true)),
      opt[String]('d', "dfs")
        .validate {
          case "0" | "1" => success
          case _ => failure("Set 'dfs 0' for BFS or 'dfs 1' for DFS")
        }
        .action((x, c) => c.copy(dfs = x  == "1")),
      opt[String]('t', "termination")
        .validate {
          case termination if TerminationCriterion.values.map(_.toString).contains(termination.toUpperCase) => success
          case _ => failure(s"Termination criterion must be one of ${TerminationCriterion.values.mkString(", ")}.")
        }
        .action((x, c) => c.copy(termination = TerminationCriterion.valueOf(x.toUpperCase))),
      opt[String] ('a', "advancement")
        .validate {
          case advancement if AdvancementType.values.map(_.toString).contains(advancement.toUpperCase) => success
          case _ => failure(s"Advancement type must be one of ${AdvancementType.values.mkString(", ")}.")
        }
        .action((x, c) => c.copy(advancement = AdvancementType.valueOf(x.toUpperCase))),
      opt[String]('o', "ordering")
        .validate {
          case ordering if s"${ordering}of1".toUpperCase.grouped(3).toSet == MoveType.values.map(_.toString).toSet => success
          case _ => failure(s"Specify the ordering of moves (without OF1) as a single, non-separated string, e.g. pf1pb2pf2pb1ob2of2ob1")
        }
        .action((x, c) => c.copy(moveTypeOrdering = x.toUpperCase.grouped(3).map(MoveType.valueOf).toSeq))
    )
  }

  def parse(args: Array[String]): Option[CliParserConfig] = OParser.parse(parser, args, CliParserConfig())

}
