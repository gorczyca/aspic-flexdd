package aspic.framework





case class Framework(strictRules: Set[Rule],
                     defeasibleRules: Set[Rule],
                     contraries: Set[Contrary],
                     axioms: Set[String],
                     ordinaryPremises: Set[String],
                     goals: Set[String]) {

//  lazy val (strictRules, defeasibleRules) = rules.partition(_.isStrict)
    lazy val rules: Set[Rule] = strictRules union defeasibleRules

    lazy val premises: Set[String] = axioms union ordinaryPremises


    def getInconsistentRulesBasedOnOrdinaryPremisesInTheirBodies(rules: Set[Rule]): Set[Rule] = rules.filter(rule => (rule.statements intersect (rule.body intersect ordinaryPremises).contraries(this)).nonEmpty)

    lazy val inconsistentStrictRules: Set[Rule] = getInconsistentRulesBasedOnOrdinaryPremisesInTheirBodies(strictRules)

    lazy val inconsistentDefeasibleRules: Set[Rule] = getInconsistentRulesBasedOnOrdinaryPremisesInTheirBodies(defeasibleRules) ++ defeasibleRules.filter(rule => (rule.statements intersect Set(rule).labelContraries(this)).nonEmpty)

    // TODO:
    lazy val inconsistentStatements: Set[String] = contraries.filter(ctr => Set(ctr.statement).contraries(this).contains(ctr.statement)).map(_.statement)

}

extension (statements: Set[String])
    // TODO: this can be more complicated if more than 1 contrary for each statement is possible
    // return image of contraries (contraries)
    def contraries(implicit framework: Framework): Set[String] = framework.contraries.filter(ctr => statements.contains(ctr.statement)).map(_.statementContrary)
    // return preimage of contraries (assumptions)
    def contrariesOf(implicit framework: Framework): Set[String] = framework.contraries.filter(ctr => statements.contains(ctr.statementContrary)).map(_.statement)

    def ruleContrariesOf(implicit framework: Framework): Set[Rule] = framework.defeasibleRules.filter(_.label match
        case Some(label) => (Set(label).contraries intersect statements).nonEmpty
        case _ => false
    )


extension (rules: Set[Rule])
    def labelContraries(implicit framework: Framework): Set[String] = rules.filterNot(_.isStrict).map(_.label.get).contraries

