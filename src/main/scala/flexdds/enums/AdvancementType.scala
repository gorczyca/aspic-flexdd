package flexdds.enums

import flexdds.dds.DisputeState
import aspic.framework.{ Rule, Framework }




enum AdvancementType(val allowedMoves: Map[Set[MoveType], MoveExtractor]) {



  case DAB extends AdvancementType(allowedMoves = Map(
    // PB1, PB2 move
    Set(MoveType.PB1, MoveType.PB2) ->
      MoveExtractor.RuleExtractor((_, framework) => framework.defeasibleRules),

    // PF2
    Set(MoveType.PF2) -> MoveExtractor(),

    // OB1, OB2
    Set(MoveType.OB1, MoveType.OB2) -> MoveExtractor(),

    // OF2
    Set(MoveType.OF2) -> MoveExtractor()
  ))



  // ----
  case DABF extends AdvancementType(allowedMoves = Map(
    // PB1, PB2 move
    Set(MoveType.PB1, MoveType.PB2) -> MoveExtractor.RuleExtractor((_, framework) => framework.defeasibleRules),

    // PF1, PF2
    Set(MoveType.PF2) ->  MoveExtractor(),

    // OB1, OB2
    Set(MoveType.OB1, MoveType.OB2) ->  MoveExtractor(),

    // OF2
    Set(MoveType.OF2) ->  MoveExtractor(),
  ))


  // ---
  case DC extends AdvancementType(allowedMoves = Map(
    // PB1, PB2 move
    Set(MoveType.PB1, MoveType.PB2) -> MoveExtractor.RuleExtractor((_, framework) => framework.defeasibleRules),

    // PF1, PF2
    Set(MoveType.PF2) -> MoveExtractor(
      statementExtractor = ???, // TODO here
      rulesExtractor = ??? // TODO: here
    ),

    // OB1, OB2
    Set(MoveType.OB1, MoveType.OB2) -> MoveExtractor(
      statementExtractor = ???, // TODO
      rulesExtractor = ???  // TODO: this prob. stays empty
    ),

    // OF2
    Set(MoveType.OF2) -> MoveExtractor(
      statementExtractor = ???, // TODO
      rulesExtractor = ??? // TODO
    )
  ))


  case DS extends AdvancementType(allowedMoves = Map(

  ))
//  DF

}