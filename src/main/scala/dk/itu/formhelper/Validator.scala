package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object Validator {
  def fieldEval(rule: Rule, field: Field, form: Form): Option[String] = field match {
    case Radio(_, _, _, _) =>
      if (ruleList(rule).contains(Required)) {
        val anyRadioFromGroupChecked = !form.fields.filter(f => f.name == field.name).filter(f => styleList(f.style).contains(Checked)).isEmpty
        if (anyRadioFromGroupChecked)None
        else Some(rule.error)
      } else None
    case Submit(_, _, _, _) => None
    case _ => {
      sealed abstract class Operation
      case object Below extends Operation
      case object BelowOrEqual extends Operation
      case object Above extends Operation
      case object AboveOrEqual extends Operation
      case object Equal extends Operation

      def opHelper[T](val1: T, val2: T, op: Operation) = (val1, val2) match {
        case (v1: Int, v2: Int) => op match {
          case Below => v1 < v2
          case BelowOrEqual => v1 <= v2
          case Above => v1 > v2
          case AboveOrEqual => v1 >= v2
          case Equal => v1 == v2
        }
        case (v1: Double, v2: Double) => op match {
          case Below => v1 < v2
          case BelowOrEqual => v1 <= v2
          case Above => v1 > v2
          case AboveOrEqual => v1 >= v2
          case Equal => v1 == v2
        }
        case (v1: String, v2: String) => op match {
          case Below => v1 < v2
          case BelowOrEqual => v1 <= v2
          case Above => v1 > v2
          case AboveOrEqual => v1 >= v2
          case Equal => v1 == v2
        }
      }

      def refHelper[T](fieldRef: FieldRef, value: T, fieldValue: T, op: Operation) = fieldRef match {
        case ThisField => opHelper(fieldValue, value, op)
        case FieldId(refId) =>
          val booleans = for {
            otherField <- form.fields.filter(f => f.id == refId)
            otherValue = otherField.value
          } yield opHelper(otherValue, value, op)
          !booleans.contains(false)
      }
      rule match {

        case <(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, Below)) None else Some(rule.error)
        case <=(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, BelowOrEqual)) None else Some(rule.error)
        case >(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, Above)) None else Some(rule.error)
        case >=(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, AboveOrEqual)) None else Some(rule.error)
        case ===(Length(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.length, Equal)) None else Some(rule.error)

        case <(Value(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.toInt, Below)) None else Some(rule.error)
        case <=(Value(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.toInt, BelowOrEqual)) None else Some(rule.error)
        case >(Value(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.toInt, Above)) None else Some(rule.error)
        case >=(Value(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.toInt, AboveOrEqual)) None else Some(rule.error)
        case ===(Value(ref), Const(n: Int)) => if (refHelper(ref, n, field.value.toInt, Equal)) None else Some(rule.error)

        case <(Value(ref), Const(n: Double)) => if (refHelper(ref, n, field.value.toDouble, Below)) None else Some(rule.error)
        case <=(Value(ref), Const(n: Double)) => if (refHelper(ref, n, field.value.toDouble, BelowOrEqual)) None else Some(rule.error)
        case >(Value(ref), Const(n: Double)) => if (refHelper(ref, n, field.value.toDouble, Above)) None else Some(rule.error)
        case >=(Value(ref), Const(n: Double)) => if (refHelper(ref, n, field.value.toDouble, AboveOrEqual)) None else Some(rule.error)
        case ===(Value(ref), Const(n: Double)) => if (refHelper(ref, n, field.value.toDouble, Equal)) None else Some(rule.error)

        case <(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, Below)) None else Some(rule.error)
        case <=(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, BelowOrEqual)) None else Some(rule.error)
        case >(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, Above)) None else Some(rule.error)
        case >=(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, AboveOrEqual)) None else Some(rule.error)
        case ===(Value(ref), Const(n: String)) => if (refHelper(ref, n, field.value, Equal)) None else Some(rule.error)

        case OK(err) => None
        case FAIL(err) => Some(err)
        case Required(err) => if (!field.value.isEmpty) None else Some(err)

        case AndRule(r1, r2) => if (fieldEval(r1, field, form) == None) if (fieldEval(r2, field, form) == None) None else Some(r2.error) else Some(r1.error)

        case ErrorRule(rule, err) => if (fieldEval(rule, field, form) == None) None else Some(err)

        case _ => Some("unknown error in Validator.Scala")
      }
    }
  }
}