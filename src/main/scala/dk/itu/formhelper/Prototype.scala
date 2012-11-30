package dk.itu.formhelper

import scala.language.implicitConversions

trait Prototype {
  sealed abstract class FieldRef {
    def name: String
  }
  case object ThisField extends FieldRef  {
    def name = "this field" //TODO: Get field label here
  }
  case class FieldId(id: String) extends FieldRef {
    def name = id // TODO: Get field label here
  }
  
  sealed abstract class Expr[T] {
    def name: String
    
    def ===(other: Expr[T]): Rule = new ===(this, other)
  }
  case class Const[T](value: T) extends Expr[T] {
    def name = value.toString
  }
  
  case class Length(field: FieldRef) extends Expr[Int] {
    def name = field.name
  }
  object Length extends Length(ThisField)
  
  sealed abstract class Rule {
    def error: String
    def withError(err: => String) = ErrorRule(this, err)
    def &&(rule: Rule) = AndRule(this, rule)
  }
  case class ===[T](e1: Expr[T], e2: Expr[T]) extends Rule {
    def error = e1.name + " must be equal to " + e2.name
  }
  case class ErrorRule(r: Rule, error: String) extends Rule
  case class AndRule(r1: Rule, r2: Rule) extends Rule {
    def error: String = r1.error + " and " + r2.error
  }
  
  val lengthIs5: Rule = (Length === 5 withError "Length must be five") && Length === 2 withError "Length must be two"
  implicit def lengthConversion(n: Int): Const[Int] = Const(n)
}