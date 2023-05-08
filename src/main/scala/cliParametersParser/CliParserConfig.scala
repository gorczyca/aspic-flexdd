package cliParametersParser

case class CliParserConfig(inputFilePath: String = "",
                           inputFormat: String = "aspicp",
                           goal: Option[String] = None)
