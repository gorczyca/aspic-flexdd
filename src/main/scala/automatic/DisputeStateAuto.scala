package automatic

import aspic.framework.Rule
import flexdds.dds.{DisputeState, DisputeStateDelta}
import flexdds.enums.MoveType


object DisputeStateAuto {
  def apply(state: DisputeState): DisputeStateAuto = DisputeStateAuto(state, Set.empty, Set.empty, List.empty)

}

case class DisputeStateAuto(state: DisputeState,
                            // defeasible pieces that have been chosen not to be adopted by the proponent
                            ignoredOrdinaryPremises: Set[String],

                            // ignored ordinary premises (culprit candidates), ignored labels and heads of defeasible rules (rule culprit candidates)
                            ignoredNotAttackedStatements: Set[String],
                            // TODO: probably won't be needed
                            // ignoredDefeasibleRules: Set[Rule],
                            // defeasible pieces that have been chosen not to be attacked by the proponent
                            // ignoredOrdinaryPremiseCulpritCandidates: Set[String],
                            // ignoredDefeasibleRuleCulpritCandidates: Set[Rule],
                            //
                            performedMoves: List[(DisputeStateDelta, MoveType)]
                           )
