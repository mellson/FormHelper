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

  final case class Value(min: Double, max: Double, equals: Double, valType: ValidationType) extends Rule {
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
      def validateHelper(sValue: Double) = {
        if (equals > 0 && equals != min && equals != max) sValue == equals
        else if (equals > 0 && equals == min) sValue >= equals
        else if (equals > 0 && equals == max) sValue <= equals
        else if (max > 0) sValue < max && sValue > min
        else sValue > min
      }
      getValue(s)
    }
  }
  
  trait ValueHelper {
    val valType: ValidationType
    def >(n: Double) = Value(min = n, max = -1, equals = -1, valType)
    def >=(n: Double) = Value(min = n, max = -1, equals = n, valType)
    def <(n: Double) = Value(min = -1, max = n, equals = -1, valType)
    def <=(n: Double) = Value(min = -1, max = n, equals = n, valType)
    def ==(n: Double) = Value(min = -1, max = -1, equals = n, valType)
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