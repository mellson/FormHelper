package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object Validator {
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
}