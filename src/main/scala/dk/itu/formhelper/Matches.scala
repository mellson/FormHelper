package dk.itu.formhelper

trait Matches {
  trait Match {
    def fieldId: String
  }
  
  trait CompareType
  case object Below extends CompareType
  case object BelowOrEqual extends CompareType
  case object Above extends CompareType
  case object AboveOrEqual extends CompareType
  case object Equal extends CompareType

  final case class Matcher(fieldId: String, comparison: CompareType, matchType: MatchHelper) extends Match
  
  trait MatchHelper {
    def <(fieldId: String) = Matcher(fieldId, Below, this)
    def <=(fieldId: String) = Matcher(fieldId, BelowOrEqual, this)
    def >(fieldId: String) = Matcher(fieldId, Above, this)
    def >=(fieldId: String) = Matcher(fieldId, AboveOrEqual, this)
    def ==(fieldId: String) = Matcher(fieldId, Equal, this)
  }
  
  case object Length extends MatchHelper
  case object Value extends MatchHelper
  
}