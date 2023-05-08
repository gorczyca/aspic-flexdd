package aspic.framework





case class Framework(strictRules: Set[Rule],
                     defeasibleRules: Set[Rule],
                     contraries: Set[Contrary],
                     axioms: Set[String],
                     ordinaryPremises: Set[String],
                     goals: Set[String]) {

//  lazy val (strictRules, defeasibleRules) = rules.partition(_.isStrict)
    lazy val rules: Set[Rule] = strictRules union defeasibleRules

    lazy val inconsistentRules: Set[Rule] = strictRules.filter(rule => (rule.statements intersect (rule.body intersect ordinaryPremises).contraries(this)).nonEmpty)


}

extension (statements: Set[String])
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


