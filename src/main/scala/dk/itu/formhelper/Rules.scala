package dk.itu.formhelper

import scala.util.matching.Regex

trait Rules {
  trait Rule {
    def validate(s: String): Boolean
  }

  case class Regex(regex: String, shouldMatch: Boolean) extends Rule {
    def validate(s: String): Boolean = if (shouldMatch) s.matches(regex) else !s.matches(regex)
  }

  object Regex extends Rule {
    def validate(s: String) = Regex.validate(s)
    def ==(s: String) = Regex(s, true)
    def !=(s: String) = Regex(s, false)
  }

  object Email extends Regex("""^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$""", true)
  object IP extends Regex("""^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$""", true)
  object Integer extends Regex("""^-?\d+$""", true)
  object Float extends Regex("""^-?(?:\d+|\d*\.\d+)$""", true)
  object Alpha extends Regex("""^\D+$""", true)

  trait ValidationType
  case object IntValidation extends ValidationType
  case object FloatValidation extends ValidationType
  case object LengthValidation extends ValidationType

  final case class Value[T](minVal: T, maxVal: T, equalVal: T, valType: ValidationType)(implicit num : Numeric[T]) extends Rule {
    def validate(s: String): Boolean = {
      def getValue(s: String) = valType match {
        case IntValidation =>
          if (!Integer.validate(s)) false
          else validateHelper(s.toInt)
        case FloatValidation =>
          if (!Float.validate(s)) false
          else validateHelper(s.toDouble)
        case LengthValidation =>
          validateHelper(s.length)
      }
      def validateHelper(sValue: T) = {
        import num._
        if (equalVal > num.zero && equalVal != minVal && equalVal != maxVal) sValue == equalVal
        else if (equalVal > num.zero && equalVal == minVal) sValue >= equalVal
        else if (equalVal > num.zero && equalVal == maxVal) sValue <= equalVal
        else if (maxVal > num.zero) sValue < maxVal && sValue > minVal
        else sValue > minVal
      }
      getValue(s)
    }
  }
  
  trait ValueHelper {
    val valType: ValidationType
    def >(n: Double) = Value(minVal = n, maxVal = -1, equalVal = -1, valType)
    def >=(n: Double) = Value(minVal = n, maxVal = -1, equalVal = n, valType)
    def <(n: Double) = Value(minVal = -1, maxVal = n, equalVal = -1, valType)
    def <=(n: Double) = Value(minVal = -1, maxVal = n, equalVal = n, valType)
    def ==(n: Double) = Value(minVal = -1, maxVal = -1, equalVal = n, valType)
  }

  object IntValue extends ValueHelper {
    val valType = IntValidation
  }
  
  object FloatValue extends ValueHelper {
    val valType = FloatValidation
  }

  object Length extends ValueHelper {
    val valType = LengthValidation
  }
  
  final case class StringValue(string: String) extends Rule {
     def validate(s: String) = string == s
  }
  
  object StringValue {
    def ==(s: String) = StringValue(s)
  }

  case object Required extends Rule {
    def validate(s: String) = !s.isEmpty()
  }

  // TODO Need to work on this
  import dk.itu.formhelper.FormHelper.Field
  final case class Matches(field: Field) extends Rule {
    def validate(s: String) = false
  }
}