package cliParametersParser

import flexdds.enums.AdvancementType
import flexdds.enums.AdvancementType.{DABF}
import flexdds.enums.TerminationCriterion
import flexdds.enums.TerminationCriterion.{TA}

case class CliParserConfig(inputFilePath: String = "",
                           inputFormat: String = "aspicp",
                           goal: Option[String] = None,
                           solve: Boolean = false,
                           advancement: AdvancementType = DABF,
                           termination: TerminationCriterion = TA
                           )
