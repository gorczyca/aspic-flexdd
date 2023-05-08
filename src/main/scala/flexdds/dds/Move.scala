package flexdds.dds

import aspic.framework.Rule

case class Move(proponentStatement: Option[String],
                proponentRule: Option[Rule],
                opponentStatement: Option[String],
                opponentRule: Option[Rule]) {

}
