package dk.itu.formhelper

import scala.util.matching.Regex

trait Rules {
  trait Rule
  
  case class Regex(regex: String, shouldMatch: Boolean) extends Rule {
    def verify(s: String): Boolean = if (shouldMatch) s.matches(regex) else !s.matches(regex)
  }
  
  object Regex extends Rule {
    def ==(s: String) = Regex(s, true)
    def !=(s: String) = Regex(s, false)
  }
  
  object Email extends Regex("""^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$""", true)
  object IP extends Regex("""^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$""", true)
  object Integer extends Regex("""^-?\d+$""", true)
  object Float extends Regex("""^-?(?:\d+|\d*\.\d+)$""", true)
  object Alpha extends Regex("""^\D+$""", true)
  
  final case class Length(min: Int, max: Int, equals: Int) extends Rule {
    def verify(s: String): Boolean = {
      if (equals > 0) s.length == equals
      else if (max > 0) s.length < max && s.length > min
      else s.length > min
    }
  }

  object Length extends Rule {
    def >(n: Int) = Length(min = n, max = 0, equals = 0)
    def <(n: Int) = Length(min = 0, max = n, equals = 0)
    def ==(n: Int) = Length(min = 0, max = 0, equals = n)
  }

  case object Required extends Rule

  final case class Matches[T](field: T) extends Rule
}