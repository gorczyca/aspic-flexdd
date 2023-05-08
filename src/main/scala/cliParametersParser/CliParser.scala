package cliParametersParser

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
        .text("Input format. Possible values are aba (default), apx and iccma")
        .valueName("<input format>"),
      opt[String]('g', "goal")
        .action((x, c) => c.copy(goal = Some(x))),
    )
  }

  def parse(args: Array[String]): Option[CliParserConfig] = OParser.parse(parser, args, CliParserConfig())

}
