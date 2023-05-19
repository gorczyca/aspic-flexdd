package cliParametersParser

import flexdds.enums.{AdvancementType, MoveType, TerminationCriterion}
import flexdds.enums.AdvancementType.DABF
import flexdds.enums.MoveType.{PF1, PB2, PF2, PB1, OB2, OF2, OB1}
import flexdds.enums.TerminationCriterion.TA

case class CliParserConfig(inputFilePath: String = "",
                           inputFormat: String = "aspicp",
                           goal: Option[String] = None,
                           solve: Boolean = false,
                           advancement: AdvancementType = DABF,
                           termination: TerminationCriterion = TA,
                           // automatic solver settings
                           moveTypeOrdering: Seq[MoveType] = Seq(PF1, PB2, PF2, PB1, OB2, OF2, OB1),
                           dfs: Boolean = true
                           )
