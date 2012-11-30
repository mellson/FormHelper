package dk.itu.formhelper

trait Matches {
  trait Match {
    def fieldId: String
  }
  
  trait MatchType
  case object LengthMatch extends MatchType
  case object ValueMatch extends MatchType
  
  trait CompareType
  case object Below extends CompareType
  case object BelowOrEqual extends CompareType
  case object Above extends CompareType
  case object AboveOrEqual extends CompareType
  case object Equal extends CompareType

  final case class Matcher(fieldId: String, comparison: CompareType, matchType: MatchType) extends Match
  
  trait MatchHelper {
    val matchType: MatchType
    def <(fieldId: String) = Matcher(fieldId, Below, matchType)
    def <=(fieldId: String) = Matcher(fieldId, BelowOrEqual, matchType)
    def >(fieldId: String) = Matcher(fieldId, Above, matchType)
    def >=(fieldId: String) = Matcher(fieldId, AboveOrEqual, matchType)
    def ==(fieldId: String) = Matcher(fieldId, Equal, matchType)
  }
  
  object Length extends MatchHelper {
    val matchType = LengthMatch
  }
  
  object Value extends MatchHelper {
    val matchType = ValueMatch
  }
}