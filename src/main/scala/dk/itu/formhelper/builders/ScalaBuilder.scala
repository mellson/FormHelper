package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object ScalaBuilder {
  // Validates a form. Returns true if the form validated ok and false if not.
  def validateForm(form: Form): Boolean = {
    val errOptions = (for {
      field <- form.fields
    } yield validateField(field.rule.getOrElse(EmptyRule), field, form)).toList

    def errorsIn(errs: List[Option[String]]): Boolean = errs match {
      case Nil => false
      case None :: xs => errorsIn(xs)
      case Some(err) :: xs => true
    }

    !errorsIn(errOptions)
  }

  // Evaluates a field and return an Option[Error String]. It returns None if the field validated.
  def validateField(rule: Rule, field: Field, form: Form): Option[String] = field match {
    case Radio(_, _, _, _) =>
      if (ruleList(rule).contains(Required)) {
        val anyRadioFromGroupChecked = !form.fields.filter(f => f.name == field.name).filter(f => styleList(f.style.getOrElse(EmptyStyle)).contains(Checked)).isEmpty
        if (anyRadioFromGroupChecked) None
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

      // Compares the values based on the operation
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

      // Gets the value from the FieldRef and evaluates it with operation given
      def refHelper[T](fieldRef: FieldRef, fieldValue: T, op: Operation, stringToValue: String => T) = fieldRef match {
        case ThisField => false
        case FieldId(refId) =>
          val booleanList = for {
            otherField <- form.fields.filter(f => f.id == refId)
            otherValue = stringToValue(otherField.value)
          } yield opHelper(fieldValue, otherValue, op)
          !booleanList.contains(false)
      }
      rule match {
        case <(Length(ThisField), Const(n: Int)) => if (field.value.length < n) None else Some(rule.error)
        case <=(Length(ThisField), Const(n: Int)) => if (field.value.length <= n) None else Some(rule.error)
        case >(Length(ThisField), Const(n: Int)) => if (field.value.length > n) None else Some(rule.error)
        case >=(Length(ThisField), Const(n: Int)) => if (field.value.length >= n) None else Some(rule.error)
        case ===(Length(ThisField), Const(n: Int)) => if (field.value.length == n) None else Some(rule.error)
        case <(Length(ThisField), Length(ref)) => if (refHelper(ref, field.value.length, Below, (s: String) => s.length)) None else Some(rule.error)
        case <=(Length(ThisField), Length(ref)) => if (refHelper(ref, field.value.length, BelowOrEqual, (s: String) => s.length)) None else Some(rule.error)
        case >(Length(ThisField), Length(ref)) => if (refHelper(ref, field.value.length, Above, (s: String) => s.length)) None else Some(rule.error)
        case >=(Length(ThisField), Length(ref)) => if (refHelper(ref, field.value.length, AboveOrEqual, (s: String) => s.length)) None else Some(rule.error)
        case ===(Length(ThisField), Length(ref)) => if (refHelper(ref, field.value.length, Equal, (s: String) => s.length)) None else Some(rule.error)
        case <(Value(ThisField), Const(n: Int)) => if (field.value.toInt < n) None else Some(rule.error)
        case <=(Value(ThisField), Const(n: Int)) => if (field.value.toInt <= n) None else Some(rule.error)
        case >(Value(ThisField), Const(n: Int)) => if (field.value.toInt > n) None else Some(rule.error)
        case >=(Value(ThisField), Const(n: Int)) => if (field.value.toInt >= n) None else Some(rule.error)
        case ===(Value(ThisField), Const(n: Int)) => if (field.value.toInt == n) None else Some(rule.error)
        case <(Value(ThisField), Const(n: Double)) => if (field.value.toDouble < n) None else Some(rule.error)
        case <=(Value(ThisField), Const(n: Double)) => if (field.value.toDouble <= n) None else Some(rule.error)
        case >(Value(ThisField), Const(n: Double)) => if (field.value.toDouble > n) None else Some(rule.error)
        case >=(Value(ThisField), Const(n: Double)) => if (field.value.toDouble >= n) None else Some(rule.error)
        case ===(Value(ThisField), Const(n: Double)) => if (field.value.toDouble == n) None else Some(rule.error)
        case <(Value(ThisField), Const(n: String)) => if (field.value < n) None else Some(rule.error)
        case <=(Value(ThisField), Const(n: String)) => if (field.value <= n) None else Some(rule.error)
        case >(Value(ThisField), Const(n: String)) => if (field.value > n) None else Some(rule.error)
        case >=(Value(ThisField), Const(n: String)) => if (field.value >= n) None else Some(rule.error)
        case ===(Value(ThisField), Const(n: String)) => if (field.value == n) None else Some(rule.error)
        case <(Value(ThisField), Value(ref)) => if (refHelper(ref, field.value, Below, (s: String) => s)) None else Some(rule.error)
        case <=(Value(ThisField), Value(ref)) => if (refHelper(ref, field.value, BelowOrEqual, (s: String) => s)) None else Some(rule.error)
        case >(Value(ThisField), Value(ref)) => if (refHelper(ref, field.value, Above, (s: String) => s)) None else Some(rule.error)
        case >=(Value(ThisField), Value(ref)) => if (refHelper(ref, field.value, AboveOrEqual, (s: String) => s)) None else Some(rule.error)
        case ===(Value(ThisField), Value(ref)) => if (refHelper(ref, field.value, Equal, (s: String) => s)) None else Some(rule.error)
        case MatchRegex(regex) => if (field.value.matches(regex)) None else Some(rule.error)
        case DoNotMatchRegex(regex) => if (!field.value.matches(regex)) None else Some(rule.error)
        case OK(err) => None
        case FAIL(err) => Some(err)
        case Required(err) => if (!field.value.isEmpty) None else Some(err)
        case AndRule(r1, r2) => if (validateField(r1, field, form) == None) if (validateField(r2, field, form) == None) None else Some(r2.error) else Some(r1.error)
        case ErrorRule(rule1, err) => if (validateField(rule1, field, form) == None) None else Some(err)
        // If the rule is Empty, it is because there were no requirements
        case EmptyRule(err) => None
        case x => Some("unknown error in Validator.Scala" + x)
      }
    }
  }

  // Converts HTTP Post data to a FormHelper.Form
  def formFromPost(form: Form, postData: Option[Map[String, Seq[String]]]): Form = {
    // Checks if the data value should be added. Need this check because Radio fields are a bit weird.
    def dataHelper(field: Field, data: String, name: String): Field = field match {
      case Radio(groupName, value, _, _) =>
        // Add checked to the radio that is checked
        if (groupName == name && data == value) field setValue data
        else field
      case _ => field setValue data
    }

    /* If a field is a member of a group that has a required rule, add that rule to all members of the group
     * The method takes a field and returns a field with the added rule if necessary
     */
    def addGroupRequired(field: Field): Field = field match {
      case Radio(_, _, _, _) =>
        val groupContainsRequired = !form.fields.filter(f => f.name == field.name).filter(f => ruleList(f.rule.getOrElse(EmptyRule)).contains(Required)).isEmpty
        val selfContainsRequired = ruleList(field.rule.getOrElse(EmptyRule)).contains(Required)
        if (groupContainsRequired)
          if (selfContainsRequired) field
          else field withRule Required
        else field
      case _ => field
    }

    // Get all the fields that has data in the post
    val dataFields = for {
      dataMap <- postData.toList
      (name, dataSeq) <- dataMap
      fields = form.fields.filter(field => field.name == name)
      field <- fields
      data <- dataSeq
      fieldWithData = dataHelper(field, data, name)
    } yield fieldWithData

    // If there was fields with data combine them with the rest of the fields
    if (!dataFields.isEmpty) {
      val newFields = for {
        field <- form.fields
        newFields = if (!dataFields.filter(f => f.id == field.id).isEmpty) dataFields.filter(f => f.id == field.id) else List(field)
        newField <- newFields
      } yield newField

      // Temp form with the data from the post
      val tempForm = Form(form.name, form.method, form.action, newFields: _*)

      // Check if the new form with data fulfills all requirements, if not add error style
      val errorFields = for {
        field <- form.fields.map(f => addGroupRequired(f))
        newFields = if (!dataFields.filter(f => f.id == field.id).isEmpty) dataFields.filter(f => f.id == field.id) else List(field)
        newField <- newFields
        error = validateField(newField.rule.getOrElse(EmptyRule), newField, tempForm)
        f = if (error == None) newField else newField withStyle Error === error.get
      } yield f

      Form(form.name, form.method, form.action, errorFields: _*)
    } else form
  }
}