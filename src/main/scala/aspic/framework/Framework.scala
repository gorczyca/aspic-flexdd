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


    private def getInconsistentRulesBasedOnOrdinaryPremisesInTheirBodies(rules: Set[Rule]): Set[Rule] = rules.filter(rule => (rule.statements intersect (rule.body intersect ordinaryPremises).contraries(this)).nonEmpty)

    lazy val inconsistentStrictRules: Set[Rule] = getInconsistentRulesBasedOnOrdinaryPremisesInTheirBodies(strictRules)

    lazy val inconsistentDefeasibleRules: Set[Rule] = getInconsistentRulesBasedOnOrdinaryPremisesInTheirBodies(defeasibleRules) ++ defeasibleRules.filter(rule => (rule.statements intersect Set(rule).statementContraries(this)).nonEmpty)

    // TODO:
//    lazy val inconsistentStatements: Set[String] = contraries.filter(ctr => Set(ctr.statement).contraries(this).contains(ctr.statement)).map(_.statement)
    lazy val inconsistentStatements: Set[String] = contraries.filter(ctr => ctr.statement == ctr.statementContrary).map(_.statement)

}

extension (statements: Set[String])
    // TODO: this can be more complicated if more than 1 contrary for each statement is possible
    // return image of contraries (contraries)
    def contraries(implicit framework: Framework): Set[String] = framework.contraries.filter(ctr => statements.contains(ctr.statement)).map(_.statementContrary)
    // return preimage of contraries (assumptions)
    def contrariesOf(implicit framework: Framework): Set[String] = framework.contraries.filter(ctr => statements.contains(ctr.statementContrary)).map(_.statement)

    // return rule preimage of contraries
    def ruleContrariesOf(implicit framework: Framework): Set[Rule] = framework.defeasibleRules.filter {
        case Rule(head, _, false, Some(label)) => (Set(label, head).contraries intersect statements).nonEmpty
        case _ => false
    }




extension (rules: Set[Rule])
    def labels: Set[String] = rules.filterNot(_.isStrict).map(_.label.get)

    def statementContraries(implicit framework: Framework): Set[String] = rules.filterNot(_.isStrict).foldLeft(Set.empty) {
        (accumulator, rule) => rule match
            case Rule(head, _, false, Some(label)) => accumulator ++ Set(label, head).contraries
            case _ => Set.empty
    }



