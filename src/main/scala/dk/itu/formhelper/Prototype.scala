package dk.itu.formhelper

import scala.language.implicitConversions

object Prototype {
  // BUILDER
  private val indent = "  "
  def fieldHtml(field: Field): String = {
    val htmlStart = "<input type=\"" + field.inputType + "\" name=\"" + field.name + "\""
    val htmlEnd = ">"
    val fieldInfoStart = "\n" + indent + indent + "<field_message class=\"info\">"
    val fieldErrorStart = "\n" + indent + indent + "<field_message class=\"error\">"
    val fieldEnd = "</field_message>"

    field.style match {
      case Label(label, placement) => placement match {
        case Before => label + htmlStart + htmlEnd
        case After => htmlStart + htmlEnd + label
        // HTML 5 feature
        case Inside => htmlStart + " placeholder=\"" + label + "\"" + htmlEnd
      }
      // Radio button checked
      case Checked => htmlStart + " checked" + htmlEnd

      case ShowRequirements => htmlStart + htmlEnd + fieldInfoStart + field.rule.error + fieldEnd
      case ShowErrors => htmlStart + htmlEnd + fieldErrorStart + field.rule.error + fieldEnd
      case _ => htmlStart + htmlEnd
    }
  }

  // VALIDATOR
  def fieldEval(rule: Rule, field: Field): Option[String] = {
    def refHelper[T](fieldRef: FieldRef, length: T, fieldValue: T, validator: (T, T) => Boolean) = fieldRef match {
    case ThisField => validator(fieldValue, length)
    case FieldId(refId) => validator(fieldValue, length)
  }
    rule match {
      case <(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, (x: Int, y: Int) => x < y)) None else Some(rule.error)
      case <=(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, (x: Int, y: Int) => x <= y)) None else Some(rule.error)
      case >(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, (x: Int, y: Int) => x > y)) None else Some(rule.error)
      case >=(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, (x: Int, y: Int) => x >= y)) None else Some(rule.error)
      case ===(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, (x: Int, y: Int) => x == y)) None else Some(rule.error)

      case <(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, (x: String, y: String) => x.toDouble < y.toDouble)) None else Some(rule.error)
      case <=(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, (x: String, y: String) => x.toDouble <= y.toDouble)) None else Some(rule.error)
      case >(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, (x: String, y: String) => x.toDouble > y.toDouble)) None else Some(rule.error)
      case >=(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, (x: String, y: String) => x.toDouble >= y.toDouble)) None else Some(rule.error)
      case ===(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, (x: String, y: String) => x.toDouble == y.toDouble)) None else Some(rule.error)

      case AndRule(r1, r2) => if (fieldEval(r1, field) == None) if (fieldEval(r2, field) == None) None else Some(r2.error) else Some(r1.error)

      case ErrorRule(rule, err) => if (fieldEval(rule, field) == None) None else Some(err)
      
      case _ => Some(rule.error)
    }
  }

  // FIELD
  sealed abstract class Field {
    def name: String
    def value: String
    def id: String
    def inputType: String
    //    val html: String = "<input type=\"" + this.inputType + "\" name=\"" + this.name + "\">"
    val rule: Rule
    val style: Style
  }

  case class Text(name: String = "", value: String = "", rule: Rule = null, style: Style = null) extends Field {
    val id = name
    val inputType = "text"
    def withRule(r: Rule): Text = Text(name, value, r, style)
    def withStyle(s: Style): Text = Text(name, value, rule, s)
    def setValue(v: String): Text = Text(name, v, rule, style)
  }

  // FIELDREF
  sealed abstract class FieldRef {
    def name: String
  }
  case object ThisField extends FieldRef {
    def name = "this field"
  }
  case class FieldId(id: String) extends FieldRef {
    def name = "field " + id
  }

  // STYLE
  sealed abstract class Style {
    def &&(style: Style) = AndStyle(this, style)
  }
  case class AndStyle(s1: Style, s2: Style) extends Style

  sealed trait Placement
  case object After extends Placement
  case object Before extends Placement
  case object Inside extends Placement

  final case class Label(label: String, placement: Placement) extends Style
  object Label extends Style {
    def <(label: String) = Label(label, Before)
    def >(label: String) = Label(label, After)
    def <>(label: String) = Label(label, Inside)
  }

  object SameLine extends Style
  case object ShowRequirements extends Style
  case object ShowErrors extends Style
  case object Checked extends Style

  // EXPR
  sealed abstract class Expr[+T] {
    def name: String
  }

  sealed abstract class ComparableExpr[T] extends Expr[T] {
    def <(that: Expr[T]): Rule = new <(this, that)
    def <=(that: Expr[T]): Rule = new <=(this, that)
    def >(that: Expr[T]): Rule = new >(this, that)
    def >=(that: Expr[T]): Rule = new >=(this, that)
    def ===(that: Expr[T]): Rule = new ===(this, that)
    def !==(that: Expr[T]): Rule = new !==(this, that)
  }

  implicit def liftIntToConst(n: Int): Const[Int] = Const(n)
  implicit def liftIntToConstString(n: Int): Const[String] = Const(n.toString())
  implicit def liftDoubleToConstString(n: Double): Const[String] = Const(n.toString())
  implicit def liftStringToConst(s: String): Const[String] = Const(s)
  case class Const[T](value: T) extends ComparableExpr[T] {
    def name = value.toString
  }

  implicit def liftStringToLength(id: FieldRef): Length = Length(id)
  case class Length(field: FieldRef) extends ComparableExpr[Int] {
    def name = field.name + "s length"
  }
  object Length extends Length(ThisField)

  implicit def liftStringToValueRef(id: FieldRef): Value = Value(id)
  case class Value(field: FieldRef) extends ComparableExpr[String] {
    def name = field.name + "s value"
  }
  object Value extends Value(ThisField)

  case class Regex(regex: String) extends Expr[String] {
    def name = regex
    def ===(that: Regex): Rule = new matchRegex(this, that)
    def !==(that: Regex): Rule = new dontMatchRegex(this, that)
  }

  /// RULE
  sealed abstract class Rule {
    def error: String
    def withError(err: => String) = ErrorRule(this, err)
    def &&(rule: Rule) = AndRule(this, rule)
    def andThen(rule: => Rule) = &&(rule)
  }

  // TODO are these right?
  case class OK(r: Rule, error: String) extends Rule
  case class FAIL(r: Rule, error: String) extends Rule

  case class ErrorRule(r: Rule, error: String) extends Rule
  case class AndRule(r1: Rule, r2: Rule) extends Rule {
    def error: String = r1.error + " and " + r2.error
  }

  // OPerator'S
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
  case class matchRegex(r1: Regex, r2: Regex) extends Rule {
    def error = r1.name + " must match " + r2.name
  }
  case class dontMatchRegex(r1: Regex, r2: Regex) extends Rule {
    def error = r1.name + " must not match " + r2.name
  }
}