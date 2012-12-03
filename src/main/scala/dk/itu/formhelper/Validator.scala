package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object Validator {
  def fieldEval(rule: Rule, field: Field, form: Form): Option[String] = {
    if (rule == null) None
    else field match {
      case Radio(_, _, _, _)  =>
        if (ruleList(rule).contains(Required)) {
          val anyRadioFromGroupChecked = !form.fields.filter(f => f.name == field.name).filter(f => styleList(f.style).contains(Checked)).isEmpty
          if (anyRadioFromGroupChecked) None
          else Some(rule.error)
        } else None
      case Submit(_, _, _, _) => None
      case _                  => {
        sealed abstract class Operation
        case object Below extends Operation
        case object BelowOrEqual extends Operation
        case object Above extends Operation
        case object AboveOrEqual extends Operation
        case object Equal extends Operation

        def opHelper[T](val1: T, val2: T, op: Operation) = (val1, val2) match {
          case (v1: Int, v2: Int)       => op match {
            case Below        => v1 < v2
            case BelowOrEqual => v1 <= v2
            case Above        => v1 > v2
            case AboveOrEqual => v1 >= v2
            case Equal        => v1 == v2
          }
          case (v1: Double, v2: Double) => op match {
            case Below        => v1 < v2
            case BelowOrEqual => v1 <= v2
            case Above        => v1 > v2
            case AboveOrEqual => v1 >= v2
            case Equal        => v1 == v2
          }
          case (v1: String, v2: String) => op match {
            case Below        => v1 < v2
            case BelowOrEqual => v1 <= v2
            case Above        => v1 > v2
            case AboveOrEqual => v1 >= v2
            case Equal        => v1 == v2
          }
        }

        def refHelper[T](fieldRef: FieldRef, fieldValue: T, op: Operation, stringToValue: String => T) = fieldRef match {
          case ThisField      => false
          case FieldId(refId) =>
            val booleans = for {
              otherField <- form.fields.filter(f => f.id == refId)
              otherValue = stringToValue(otherField.value)
            } yield opHelper(fieldValue, otherValue, op)
            !booleans.contains(false)
        }
        rule match {
          case <(Length(ThisField), Const(n: Int))   => if (field.value.length < n) None else Some(rule.error)
          case <=(Length(ThisField), Const(n: Int))  => if (field.value.length <= n) None else Some(rule.error)
          case >(Length(ThisField), Const(n: Int))   => if (field.value.length > n) None else Some(rule.error)
          case >=(Length(ThisField), Const(n: Int))  => if (field.value.length >= n) None else Some(rule.error)
          case ===(Length(ThisField), Const(n: Int)) => if (field.value.length == n) None else Some(rule.error)

          case <(Length(ThisField), Length(ref))   => if (refHelper(ref, field.value.length, Below, (s: String) => s.length)) None else Some(rule.error)
          case <=(Length(ThisField), Length(ref))  => if (refHelper(ref, field.value.length, BelowOrEqual, (s: String) => s.length)) None else Some(rule.error)
          case >(Length(ThisField), Length(ref))   => if (refHelper(ref, field.value.length, Above, (s: String) => s.length)) None else Some(rule.error)
          case >=(Length(ThisField), Length(ref))  => if (refHelper(ref, field.value.length, AboveOrEqual, (s: String) => s.length)) None else Some(rule.error)
          case ===(Length(ThisField), Length(ref)) => if (refHelper(ref, field.value.length, Equal, (s: String) => s.length)) None else Some(rule.error)

          case <(Value(ThisField), Const(n: Int))   => if (field.value.toInt < n) None else Some(rule.error)
          case <=(Value(ThisField), Const(n: Int))  => if (field.value.toInt <= n) None else Some(rule.error)
          case >(Value(ThisField), Const(n: Int))   => if (field.value.toInt > n) None else Some(rule.error)
          case >=(Value(ThisField), Const(n: Int))  => if (field.value.toInt >= n) None else Some(rule.error)
          case ===(Value(ThisField), Const(n: Int)) => if (field.value.toInt == n) None else Some(rule.error)

          case <(Value(ThisField), Const(n: Double))   => if (field.value.toDouble < n) None else Some(rule.error)
          case <=(Value(ThisField), Const(n: Double))  => if (field.value.toDouble <= n) None else Some(rule.error)
          case >(Value(ThisField), Const(n: Double))   => if (field.value.toDouble > n) None else Some(rule.error)
          case >=(Value(ThisField), Const(n: Double))  => if (field.value.toDouble >= n) None else Some(rule.error)
          case ===(Value(ThisField), Const(n: Double)) => if (field.value.toDouble == n) None else Some(rule.error)

          case <(Value(ThisField), Const(n: String))   => if (field.value < n) None else Some(rule.error)
          case <=(Value(ThisField), Const(n: String))  => if (field.value <= n) None else Some(rule.error)
          case >(Value(ThisField), Const(n: String))   => if (field.value > n) None else Some(rule.error)
          case >=(Value(ThisField), Const(n: String))  => if (field.value >= n) None else Some(rule.error)
          case ===(Value(ThisField), Const(n: String)) => if (field.value == n) None else Some(rule.error)

          case <(Value(ThisField), Value(ref))   => if (refHelper(ref, field.value, Below, (s: String) => s)) None else Some(rule.error)
          case <=(Value(ThisField), Value(ref))  => if (refHelper(ref, field.value, BelowOrEqual, (s: String) => s)) None else Some(rule.error)
          case >(Value(ThisField), Value(ref))   => if (refHelper(ref, field.value, Above, (s: String) => s)) None else Some(rule.error)
          case >=(Value(ThisField), Value(ref))  => if (refHelper(ref, field.value, AboveOrEqual, (s: String) => s)) None else Some(rule.error)
          case ===(Value(ThisField), Value(ref)) => if (refHelper(ref, field.value, Equal, (s: String) => s)) None else Some(rule.error)

          case MatchRegex(regex)     => if (field.value.matches(regex)) None else Some(rule.error)
          case DontMatchRegex(regex) => if (!field.value.matches(regex)) None else Some(rule.error)

          case OK(err)       => None
          case FAIL(err)     => Some(err)
          case Required(err) => if (!field.value.isEmpty) None else Some(err)

          case AndRule(r1, r2) => if (fieldEval(r1, field, form) == None) if (fieldEval(r2, field, form) == None) None else Some(r2.error) else Some(r1.error)

          case ErrorRule(rule, err) => if (fieldEval(rule, field, form) == None) None else Some(err)

          // TODO Fix error here
          case x => Some("unknown error in Validator.Scala" + x)
        }
      }
    }
  }
}