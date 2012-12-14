package dk.itu.formhelper

trait Rules {

  sealed abstract class FieldRef {
    def name: String

    def id: String
  }

  case object ThisField extends FieldRef {
    def name = "this field"

    def id = name
  }

  case class FieldId(id: String) extends FieldRef {
    def name = "field " + id
  }

  sealed abstract class Expr[+T] {
    def name: String

    def <[U >: T](that: Expr[U]): Rule = new <(this, that)

    def <=[U >: T](that: Expr[U]): Rule = new <=(this, that)

    def >[U >: T](that: Expr[U]): Rule = new >(this, that)

    def >=[U >: T](that: Expr[U]): Rule = new >=(this, that)

    def ===[U >: T](that: Expr[U]): Rule = new ===(this, that)

    def !==[U >: T](that: Expr[U]): Rule = new !==(this, that)
  }

  implicit def liftIntToConst(n: Int): Const[Int] = Const(n)

  implicit def liftDoubleToConst(n: Double): Const[Double] = Const(n)

  implicit def liftStringToConst(s: String): Const[String] = Const(s)

  case class Const[T](value: T) extends Expr[T] {
    def name = value.toString
  }

  implicit def liftStringToLength(id: FieldRef): Length = Length(id)

  case class Length(field: FieldRef) extends Expr[Int] {
    def name = field.name + "s length"
  }

  object Length extends Length(ThisField)

  implicit def liftStringToValueRef(id: FieldRef): Value[String] = Value[String](id)

  case class Value[T](field: FieldRef) extends Expr[T] {
    def name = field.name + "s value"
  }

  object Value extends Value[String](ThisField)

  object StringValue extends Value[String](ThisField)

  object IntValue extends Value[Int](ThisField) {
    override def name = field.name + "s integer value"
  }

  object DoubleValue extends Value[Double](ThisField) {
    override def name = field.name + "s double value"
  }

  sealed abstract class Rule {
    def error: String

    // Assign custom error to a rule
    def withError(err: => String) = ErrorRule(this, err)

    // Assign custom error to a rule. This method name binds stronger and can therefor be used without adding parentheses
    def |>(err: => String) = withError(err)

    def &&(rule: Rule) = AndRule(this, rule)
  }

  case class ShowWhen(id: FieldRef, rule: Rule, error: String = "") extends Rule

  case class EmptyRule(error: String) extends Rule

  object EmptyRule extends EmptyRule(error = " No Requirements ")

  case class OK(error: String) extends Rule

  object OK extends OK(error = " OK ")

  case class FAIL(error: String) extends Rule

  object FAIL extends FAIL(error = " FAIL ")

  case class Required(error: String) extends Rule

  object Required extends Required(error = "this is required")

  case class ErrorRule(r: Rule, error: String) extends Rule

  case class AndRule(r1: Rule, r2: Rule) extends Rule {
    def error: String = r1.error + " and " + r2.error
  }

  final case class MatchRegex(regex: String) extends Rule {
    def error = " must match " + regex
  }

  final case class DoNotMatchRegex(regex: String) extends Rule {
    def error = " must not match " + regex
  }

  object Regex {
    def ===(regex: String) = MatchRegex(regex)

    def !==(regex: String) = DoNotMatchRegex(regex)
  }

  // Various strings to help build regular expressions
  val Email = """^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$"""
  val IP = """^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"""
  val Integer = """^-?\d+$"""
  val Float = """^-?(?:\d+|\d*\.\d+)$"""
  val Alpha = """^\D+$"""

  // Operators
  case class <[T](e1: Expr[T], e2: Expr[T]) extends Rule {
    def error = e1.name + " must be under " + e2.name
  }

  case class <=[T](e1: Expr[T], e2: Expr[T]) extends Rule {
    def error = e1.name + " must be under or equal to " + e2.name
  }

  case class >[T](e1: Expr[T], e2: Expr[T]) extends Rule {
    def error = e1.name + " must be over " + e2.name
  }

  case class >=[T](e1: Expr[T], e2: Expr[T]) extends Rule {
    def error = e1.name + " must be over or equal to " + e2.name
  }

  case class ===[T](e1: Expr[T], e2: Expr[T]) extends Rule {
    def error = e1.name + " must be equal to " + e2.name
  }

  case class !==[T](e1: Expr[T], e2: Expr[T]) extends Rule {
    def error = e1.name + " must not be equal to " + e2.name
  }

}