package dk.itu.formhelper

//import scala.language.implicitConversions
import dk.itu.formhelper.FormHelper._
import scala.util.matching.Regex

trait Rules {
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
  implicit def liftDoubleToConst(n: Double): Const[Double] = Const(n)
  implicit def liftStringToConst(s: String): Const[String] = Const(s)
  case class Const[T](value: T) extends ComparableExpr[T] {
    def name = value.toString
  }

  implicit def liftStringToLength(id: FieldRef): Length = Length(id)
  case class Length(field: FieldRef) extends ComparableExpr[Int] {
    def name = field.name + "s length"
  }
  object Length extends Length(ThisField)

  implicit def liftStringToValueRef(id: FieldRef): Value[String] = Value[String](id)
  case class Value[T](field: FieldRef) extends ComparableExpr[T] {
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

  sealed abstract class RegexExpr extends Expr[String] {
    def ===(that: RegexExpr): Rule = new matchRegex(this, that)
    def !==(that: RegexExpr): Rule = new dontMatchRegex(this, that)
  }
  case class Regex(regex: String) extends RegexExpr {
    def name = ThisField.name + regex
  }
//  object Regex extends RegexExpr {
//    
//  }

  sealed abstract class Rule {
    def error: String
    def withError(err: => String) = ErrorRule(this, err)
    def &&(rule: Rule) = AndRule(this, rule)
    def andThen(rule: => Rule) = &&(rule)
  }

  case class OK(error:String) extends Rule
  object OK extends OK(error = " OK ")
  
  case class FAIL(error:String) extends Rule
  object FAIL extends FAIL(error = " FAIL ")
  
  case class Required(error:String) extends Rule
  object Required extends Required(error = " required ")

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
  case class matchRegex(r1: RegexExpr, r2: RegexExpr) extends Rule {
    def error = r1.name + " must match " + r2.name
  }
  case class dontMatchRegex(r1: RegexExpr, r2: RegexExpr) extends Rule {
    def error = r1.name + " must not match " + r2.name
  }
}