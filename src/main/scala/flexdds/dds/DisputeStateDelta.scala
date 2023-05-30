package flexdds.dds

import aspic.framework.{Framework, Rule, contraries, contrariesOf, ruleContrariesOf, statementContraries}
import flexdds.enums.MoveType

import scala.annotation.tailrec


sealed trait DisputeStateDelta {

  def premiseOrRuleHead: String


  def statements: Set[String]

  override def toString: String = this match
     // TODO: DRY
    case ProponentStatement(s) => s
    case ProponentRule(r) => r.toString
    case OpponentStatement(s) => s
    case OpponentRule(r) => r.toString

  def performMove(state: DisputeState)(implicit framework: Framework): DisputeState = {
    val (pStatements, pRules, oStatements, oRules) = this match
      case ProponentStatement(s) => (Set(s), Set.empty[Rule], Set.empty[String], Set.empty[Rule])
      case ProponentRule(r) => (r.statements, Set(r), Set.empty[String], Set.empty[Rule])
      case OpponentStatement(s) => (Set.empty[String], Set.empty[Rule], Set(s), Set.empty[Rule])
      case OpponentRule(r) => (Set.empty[String], Set.empty[Rule], r.statements, Set(r))

      //
      val nPStatements = state.pStatements ++ pStatements
      val nPRules = state.pRules ++ pRules
      val nOStatements = state.oStatements ++ oStatements
      val nORules = state.oRules ++ oRules



      //
      val newAdoptedOrdinaryPremises = pStatements intersect framework.ordinaryPremises
      val nAdoptedOrdinaryPremises = state.adoptedOrdinaryPremises ++ newAdoptedOrdinaryPremises
      val newAdoptedDefeasibleRules = pRules intersect framework.defeasibleRules
      val nAdoptedDefeasibleRules = state.adoptedDefeasibleRules ++ newAdoptedDefeasibleRules
      val newRejectedOrdinaryPremises = pStatements.contrariesOf intersect framework.ordinaryPremises
      val nRejectedOrdinaryPremises = state.rejectedOrdinaryPremises ++ newRejectedOrdinaryPremises
      val nRejectedDefeasibleRules = state.rejectedDefeasibleRules ++ pStatements.ruleContrariesOf  // TODO: maybe here also

      //
      val nBlockedRules = state.blockedRules ++ framework.rules.filter(rule => (rule.body intersect newRejectedOrdinaryPremises).nonEmpty)
      // this could just be done in a single step, without distinction between defeasible and strict
      val nPBlockedStrictRules = state.pBlockedStrictRules ++ framework.strictRules.filter(rule => (rule.statements intersect (newAdoptedOrdinaryPremises.contraries union newAdoptedDefeasibleRules.statementContraries)).nonEmpty)

      val nPBlockedDefeasibleRules = state.pBlockedDefeasibleRules ++ framework.defeasibleRules.filter(rule => (rule.statements intersect (newAdoptedOrdinaryPremises.contraries union newAdoptedDefeasibleRules.statementContraries)).nonEmpty) // the defeasible rules rejected because their labels attacked already covered by nRejectedDefeasibleRules
      val nPPlayedUnexpandedStatements = nPStatements.filter(stmt => !nPRules.exists(_.head == stmt))

      // aux
      val nStatements = nPStatements ++ nOStatements
      val nRules = nPRules ++ nORules
      val nRemainingNonBlockedRules = framework.rules -- (nRules ++ nBlockedRules ++ nRejectedDefeasibleRules) // TODO: the additional def. rules are blocked only for the proponent?

      val nPlayedFullyExpandedStatements = nStatements.filter(stmt => !nRemainingNonBlockedRules.exists(_.head == stmt))

      val (nBBlockedPlayedStatements, nBBlockedPlayedRules) = getPlayedBlockedPieces(
        nRejectedOrdinaryPremises intersect nStatements,
        nRejectedDefeasibleRules intersect nRules, // what about other blocked rules than defeasible ones?
        nPlayedFullyExpandedStatements -- (framework.premises ++ nRejectedOrdinaryPremises),
        nRules -- nRejectedDefeasibleRules
      )

      val (nPPlayedCompleteStatements, nPPlayedCompleteRules) = getPPlayedCompletePieces(
        state.pCompleteStatements ++ (pStatements intersect framework.premises),
        state.pCompleteRules,
        nPRules -- state.pCompleteRules
      )

      val (nBNonBlockedCompleteStatements, nBNonBlockedCompleteRules) = getBNonBlockedCompletePieces(
        nStatements intersect (framework.premises -- nRejectedOrdinaryPremises),
        nPPlayedCompleteRules,
        nRules -- nBBlockedPlayedRules,
        nStatements -- (framework.premises ++ nBBlockedPlayedStatements)
      )

      val (nBUnblockedStatementsSupportingContrariesOfAdoptedPieces, nBUnblockedRulesSupportingContrariesOfAdoptedPieces) = getUnblockedPiecesSupportingS(
        (nStatements -- nBBlockedPlayedStatements) intersect (nAdoptedOrdinaryPremises.contraries ++ nAdoptedOrdinaryPremises.contraries),
        Set.empty[Rule],
        nRules -- nBBlockedPlayedRules,
        nStatements -- nBBlockedPlayedStatements
      )

      val nCulpritCandidatesStatements = nBUnblockedStatementsSupportingContrariesOfAdoptedPieces intersect framework.ordinaryPremises
      val nCulpritCandidatesRules = nBUnblockedRulesSupportingContrariesOfAdoptedPieces intersect framework.defeasibleRules

      val nCurrentlyDefendedOrdinaryPremises = (framework.ordinaryPremises -- nRejectedOrdinaryPremises).filter(stmt => Set(stmt).contraries.intersect(nBNonBlockedCompleteStatements).isEmpty)
      val nCurrentlyDefendedDefeasibleRules = (framework.defeasibleRules -- nRejectedDefeasibleRules).filter(rule => Set(rule).statementContraries.intersect(nBNonBlockedCompleteStatements).isEmpty)

      val nCurrentlyDefendedPiecesContraries = nCurrentlyDefendedOrdinaryPremises.contraries ++ nCurrentlyDefendedDefeasibleRules.statementContraries

      val (nBUnblockedStatementsSupportingContrariesOfCurrentlyDefendedPieces, bUnblockedRulesSupportingContrariesOfCurrentlyDefendedPieces) = getUnblockedPiecesSupportingS(
        (nStatements -- nBBlockedPlayedStatements) intersect (nCurrentlyDefendedPiecesContraries),
        Set.empty[Rule],
        nRules -- nBBlockedPlayedRules,
        nStatements -- nBBlockedPlayedStatements
      )

      DisputeState(
        state.goals,
        nPStatements,
        nPRules,
        nOStatements,
        nORules,
        nAdoptedOrdinaryPremises,
        nAdoptedDefeasibleRules,
        nRejectedOrdinaryPremises,
        nRejectedDefeasibleRules,
        nBlockedRules,
        nPBlockedStrictRules,
        nPBlockedDefeasibleRules,
        nPPlayedUnexpandedStatements,
        nPlayedFullyExpandedStatements,
        nBBlockedPlayedStatements,
        nBBlockedPlayedRules,
        nPPlayedCompleteStatements,
        nPPlayedCompleteRules,
        nBNonBlockedCompleteStatements, // TODO: be consistent with naming conventions, like NonBlocked vs Unblocked etc
        nBNonBlockedCompleteRules,
        nBUnblockedStatementsSupportingContrariesOfAdoptedPieces,
        nBUnblockedRulesSupportingContrariesOfAdoptedPieces,
        nBUnblockedStatementsSupportingContrariesOfCurrentlyDefendedPieces,
        bUnblockedRulesSupportingContrariesOfCurrentlyDefendedPieces,
        nCulpritCandidatesStatements,
        nCulpritCandidatesRules,
        nCurrentlyDefendedOrdinaryPremises,
        nCurrentlyDefendedDefeasibleRules)
  }

  @tailrec
  final private def getPlayedBlockedPieces(blockedStatements: Set[String],
                                           blockedRules: Set[Rule],
                                           remainingStatements: Set[String],
                                           remainingRules: Set[Rule]): (Set[String], Set[Rule]) = {

    val (nBlockedRules, nRemainingRules) = remainingRules.partition(_.body.intersect(blockedStatements).nonEmpty)
    val (nBlockedStatements, nRemainingStatements) = remainingStatements.partition(stmt => !nRemainingRules.exists(_.head == stmt))

    if (nBlockedStatements.isEmpty && nBlockedRules.isEmpty) (blockedStatements, blockedRules)
    else getPlayedBlockedPieces(
      blockedStatements ++ nBlockedStatements,
      blockedRules ++ nBlockedRules,
      nRemainingStatements,
      nRemainingRules
    )
  }

  @tailrec
  final private def getPPlayedCompletePieces(pCompleteStatements: Set[String],
                                             pCompleteRules: Set[Rule],
                                             remainingRules: Set[Rule]): (Set[String], Set[Rule]) = {

    val (nPCompleteRules, nRemainingRules) = remainingRules.partition {
      case Rule(_, body, _, _) => body.subsetOf(pCompleteStatements)
    }

    if (nPCompleteRules.isEmpty) (pCompleteStatements, pCompleteRules)
    else getPPlayedCompletePieces(
      pCompleteStatements ++ nPCompleteRules.map(_.head),
      pCompleteRules ++ nPCompleteRules,
      nRemainingRules
    )
  }

  @tailrec
  final private def getBNonBlockedCompletePieces(bNonBlockedCompleteStatements: Set[String],
                                                 bNonBlockedCompleteRules: Set[Rule],
                                                 remainingRules: Set[Rule],
                                                 possibleStatements: Set[String]): (Set[String], Set[Rule]) = { //TODO: possible statement most likely not needed

    val (nBNonBlockedCompleteRules, nRemainingRules) = remainingRules.partition(_.body.subsetOf(bNonBlockedCompleteStatements))
    if (nBNonBlockedCompleteRules.isEmpty) (bNonBlockedCompleteStatements, bNonBlockedCompleteRules)
    else getBNonBlockedCompletePieces(
      bNonBlockedCompleteStatements ++ (nBNonBlockedCompleteRules.map(_.head) intersect possibleStatements),
      bNonBlockedCompleteRules ++ nBNonBlockedCompleteRules,
      nRemainingRules,
      possibleStatements
    )
  }

  @tailrec
  final private def getUnblockedPiecesSupportingS(bNonBlockedStatementsSupportingS: Set[String],
                                                  bNonBlockedRulesSupportingS: Set[Rule],
                                                  remainingRules: Set[Rule],
                                                  possibleStatements: Set[String]): (Set[String], Set[Rule]) = {

    val (nBNonBlockedRulesSupportingS, nRemainingRules) = remainingRules.partition(rule => bNonBlockedStatementsSupportingS.contains(rule.head))
    if (nBNonBlockedRulesSupportingS.isEmpty) (bNonBlockedStatementsSupportingS, bNonBlockedRulesSupportingS)
    else
      getUnblockedPiecesSupportingS(bNonBlockedStatementsSupportingS ++ nBNonBlockedRulesSupportingS.flatMap(_.body) intersect possibleStatements, // TODO: poss. stmts most likely not necessary
        bNonBlockedRulesSupportingS ++ nBNonBlockedRulesSupportingS,
        nRemainingRules,
        possibleStatements
      )
  }

}


sealed trait StatementMove(statement: String) extends DisputeStateDelta {
  override def premiseOrRuleHead: String = statement
  override def statements: Set[String] = Set(statement)
}
sealed trait RuleMove(rule: Rule) extends DisputeStateDelta {
  override def premiseOrRuleHead: String = rule.head
  override def statements: Set[String] = rule.statements
}

sealed trait ProponentMove extends DisputeStateDelta
sealed trait OpponentMove extends DisputeStateDelta


case class ProponentStatement(statement: String) extends StatementMove(statement) with ProponentMove

case class ProponentRule(rule: Rule) extends RuleMove(rule) with ProponentMove

case class OpponentStatement(statement: String) extends StatementMove(statement) with OpponentMove

case class OpponentRule(rule: Rule) extends RuleMove(rule) with OpponentMove


extension (performedMoves: List[(DisputeStateDelta, MoveType)])
  def performedMovesToString: String = performedMoves.zipWithIndex.map((m, index) => s"${index+1}: [${m._2}] ${m._1}").mkString("\n")


