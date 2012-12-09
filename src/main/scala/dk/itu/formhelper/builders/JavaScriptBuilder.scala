package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object JavaScriptBuilder {
  // Returns a tuple with the validation function and a unique id for each rule in a rule list
  private def ruleToFunctions(rule1: Rule, field: Field, form: Form, customErr: String = ""): List[(String, String)] = {
    def ruleHelper(rule: Rule, field: Field, form: Form, customErr: String = "", id: Int = 0): Option[(String, String)] = {
      field match {
        case Submit(_, _, _, _)                       => None
        case Radio(_, _, _, _) | Checkbox(_, _, _, _) =>
          if (ruleList(rule1).contains(Required)) {
            Some(radioRequiredValidation(field.id, field.name, field.id + id, if (customErr.isEmpty) rule.error else customErr), field.id + id)
          } else None
        case _                                        =>
          rule match {
            case ErrorRule(r, error)                     => ruleHelper(r, field, form, error, id)
            case OK(error)                               => Some((okValidation(field.id + id, if (customErr.isEmpty) error else customErr), field.id + id))
            case FAIL(error)                             => Some((failValidation(field.id + id, if (customErr.isEmpty) error else customErr), field.id + id))
            case Required(error)                         => Some((requiredValidation(field.id, field.id + id, if (customErr.isEmpty) error else customErr), field.id + id))
            case MatchRegex(regex)                       => Some((regexValidation(field.id, field.id + id, regex, if (customErr.isEmpty) rule.error else customErr, shouldMatch = true), field.id + id))
            case DoNotMatchRegex(regex)                  => Some((regexValidation(field.id, field.id + id, regex, if (customErr.isEmpty) rule.error else customErr, shouldMatch = false), field.id + id))
            case <(Length(ThisField), Const(n: Int))     => Some((lengthValidation(field.id, field.id + id, n, "<", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <=(Length(ThisField), Const(n: Int))    => Some((lengthValidation(field.id, field.id + id, n, "<=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >(Length(ThisField), Const(n: Int))     => Some((lengthValidation(field.id, field.id + id, n, ">", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >=(Length(ThisField), Const(n: Int))    => Some((lengthValidation(field.id, field.id + id, n, ">=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case ===(Length(ThisField), Const(n: Int))   => Some((lengthValidation(field.id, field.id + id, n, "==", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <(Length(ThisField), Length(ref))       => Some((lengthRefValidation(field.id, field.id + id, ref, "<", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <=(Length(ThisField), Length(ref))      => Some((lengthRefValidation(field.id, field.id + id, ref, "<=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >(Length(ThisField), Length(ref))       => Some((lengthRefValidation(field.id, field.id + id, ref, ">", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >=(Length(ThisField), Length(ref))      => Some((lengthRefValidation(field.id, field.id + id, ref, ">=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case ===(Length(ThisField), Length(ref))     => Some((lengthRefValidation(field.id, field.id + id, ref, "==", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <(Value(ThisField), Const(n: Int))      => Some((intValueValidation(field.id, field.id + id, n, "<", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <=(Value(ThisField), Const(n: Int))     => Some((intValueValidation(field.id, field.id + id, n, "<=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >(Value(ThisField), Const(n: Int))      => Some((intValueValidation(field.id, field.id + id, n, ">", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >=(Value(ThisField), Const(n: Int))     => Some((intValueValidation(field.id, field.id + id, n, ">=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case ===(Value(ThisField), Const(n: Int))    => Some((intValueValidation(field.id, field.id + id, n, "==", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <(Value(ThisField), Const(n: Double))   => Some((doubleValueValidation(field.id, field.id + id, n, "<", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <=(Value(ThisField), Const(n: Double))  => Some((doubleValueValidation(field.id, field.id + id, n, "<=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >(Value(ThisField), Const(n: Double))   => Some((doubleValueValidation(field.id, field.id + id, n, ">", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >=(Value(ThisField), Const(n: Double))  => Some((doubleValueValidation(field.id, field.id + id, n, ">=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case ===(Value(ThisField), Const(n: Double)) => Some((doubleValueValidation(field.id, field.id + id, n, "==", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <(Value(ThisField), Const(n: String))   => Some((stringValueValidation(field.id, field.id + id, n, "<", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <=(Value(ThisField), Const(n: String))  => Some((stringValueValidation(field.id, field.id + id, n, "<=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >(Value(ThisField), Const(n: String))   => Some((stringValueValidation(field.id, field.id + id, n, ">", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >=(Value(ThisField), Const(n: String))  => Some((stringValueValidation(field.id, field.id + id, n, ">=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case ===(Value(ThisField), Const(n: String)) => Some((stringValueValidation(field.id, field.id + id, n, "==", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <(Value(ThisField), Value(ref))         => Some((valueRefValidation(field.id, field.id + id, ref, "<", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case <=(Value(ThisField), Value(ref))        => Some((valueRefValidation(field.id, field.id + id, ref, "<=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >(Value(ThisField), Value(ref))         => Some((valueRefValidation(field.id, field.id + id, ref, ">", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case >=(Value(ThisField), Value(ref))        => Some((valueRefValidation(field.id, field.id + id, ref, ">=", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case ===(Value(ThisField), Value(ref))       => Some((valueRefValidation(field.id, field.id + id, ref, "==", if (customErr.isEmpty) rule.error else customErr), field.id + id))
            case _                                       => None
          }
      }
    }
    // Create a rule list from the rule supplied. This filters AndRules out, which makes it easier to create a unique id for each solution
    val rules = ruleList(rule1)
    // Get all optional solutions
    val optionalSolutions = for (i <- 0 until rules.length)
    yield ruleHelper(rules(i), field, form, customErr, i)
    // Iterate over each option and convert the real solutions to a list
    (for {
      option <- optionalSolutions
      solution <- option
    } yield solution).toList
  }

  // Extract the function name of a function
  private def functionNameExtractor(js: String) = js.substring(js.indexOf("function") + 8, js.indexOf(")") + 1).trim

  // Returns JavaScript for validating a form
  def validationScriptForForm(form: Form): String = {
    def fieldsHelper(fields: List[Field]): List[String] = fields match {
      case Nil         => Nil
      case field :: xs => field.rule match {
        case Some(rule) =>
          val ruleFunctionTuples = ruleToFunctions(rule, field, form)
          val functions = ruleFunctionTuples.map(functionAndUniqueId => functionAndUniqueId._1)
          val functionNamesAndUniqueIds = ruleFunctionTuples.map(functionAndUniqueId => (functionNameExtractor(functionAndUniqueId._1), functionAndUniqueId._2))
          functions.mkString("") + fieldValidation(functionNamesAndUniqueIds, field).mkString("") + errHelperIfLabelAfter(field) :: fieldsHelper(xs)
        case None       => fieldsHelper(xs)
      }
    }
    "\n<script>" + fieldsHelper(form.fields.toList).mkString("") + formValidationScript(form) + "\n</script>"
  }

  private def filterSubmit(field: Field) = field match {
    case Submit(_, _, _, _) => false
    case _                  => true
  }

  private def formValidationScript(form: Form): String =
    """
      |function validateForm() {
      | """.stripMargin + // Validate each field when a user tries to submit the form
      (for {field <- form.fields} yield """    document.getElementById("%s").focus();""".format(field.id)).mkString("\n") +
      """
        |    var ok = false;
        |    var formContainer = document.getElementById("%s");
        |    var inputElements = formContainer.getElementsByTagName("input");
        |    for (var i = 0; i < inputElements.length; i++) {
        |        var vf = inputElements[i].getAttribute("validField");
        |        if (vf == "notValid" || vf == "") {
        |            for (var ii = 0; ii < inputElements.length; ii++) {
        |                if (inputElements[ii].hasAttribute("validField")) {
        |                    var vf2 = inputElements[ii].getAttribute("validField");
        |                    if (vf2 != "valid")
        |                        inputElements[ii].setAttribute("validField", "notValid");
        |                }
        |
        |            }
        |            return false;
        |        }
        |        ok = true;
        |    }
        |    return ok;
        |}""".format(form.id).stripMargin

  private def functionValidationHelper(functionNameAndUniqueId: (String, String)): String =
    """
    if (!%s) {
        x.setAttribute("validField", "notValid");
        xErrField.innerHTML = %serrMsg;
        return false;
    }""".format(functionNameAndUniqueId._1, functionNameAndUniqueId._2)

  private def idHelper(field: Field) = field match {
    case Radio(_, _, _, _) | Checkbox(_, _, _, _) => field.name
    case _                                        => field.id
  }

  private def fieldValidation(functionNames: Seq[(String, String)], field: Field): String =
    """
      |function validate%s() {
      |    var xErrField = document.getElementById("errMsg%s");
      |    var x = document.getElementById("%s");""".format(idHelper(field), field.id, field.id).stripMargin +
      (for {fn <- functionNames} yield functionValidationHelper(fn)).mkString("") +
      """
        |    x.setAttribute("validField", "valid");
        |    xErrField.innerHTML = "";
        |    return true;
        |}
        | """.stripMargin

  // Create a JavaScript variable containing the error message
  def jsErrMsg(fieldId: String, errMsg: String) = "\nvar " + fieldId + "errMsg = '" + errMsg + "'\n"

  private def lengthValidation(fieldId: String, uniqueId: String, length: Int, operator: String, errMsg: String) =
    """
      |function validateLength%s() {
      |    var x = document.getElementById("%s");
      |    return x.value.length %s %d;
      |}""".format(uniqueId, fieldId, operator, length).stripMargin + jsErrMsg(uniqueId, errMsg)

  private def intValueValidation(fieldId: String, uniqueId: String, value: Int, operator: String, errMsg: String) =
    """
      |function validateIntValue%s() {
      |    var x = document.getElementById("%s");
      |    return x.value %s %d;
      |}""".format(uniqueId, fieldId, operator, value).stripMargin + jsErrMsg(uniqueId, errMsg)

  private def doubleValueValidation(fieldId: String, uniqueId: String, value: Double, operator: String, errMsg: String) =
    """
      |function validateDoubleValue%s() {
      |    var x = document.getElementById("%s");
      |    return x.value %s %f;
      |}""".format(uniqueId, fieldId, operator, value).stripMargin + jsErrMsg(uniqueId, errMsg)

  private def stringValueValidation(fieldId: String, uniqueId: String, value: String, operator: String, errMsg: String) =
    """
      |function validateStringValue%s() {
      |    var x = document.getElementById("%s");
      |    return x.value %s "%s";
      |}""".format(uniqueId, fieldId, operator, value).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Helps get the correct reference for the ref validators.
  private def jsRefHelper[T](fieldRef: FieldRef, fieldId: String): String = fieldRef match {
    case ThisField      => fieldId
    case FieldId(refId) => refId
  }

  private def lengthRefValidation(fieldId: String, uniqueId: String, ref: FieldRef, operator: String, errMsg: String) =
    """
      |function validateRefLength%s() {
      |    var x = document.getElementById("%s");
      |    var y = document.getElementById("%s");
      |    return x.value.length %s y.value.length;
      |}""".format(uniqueId, fieldId, jsRefHelper(ref, fieldId), operator).stripMargin + jsErrMsg(uniqueId, errMsg)

  private def valueRefValidation(fieldId: String, uniqueId: String, ref: FieldRef, operator: String, errMsg: String) =
    """
      |function validateRefValue%s() {
      |    var x = document.getElementById("%s");
      |    var y = document.getElementById("%s");
      |    return x.value %s y.value;
      |}""".format(uniqueId, fieldId, jsRefHelper(ref, fieldId), operator).stripMargin + jsErrMsg(uniqueId, errMsg)

  private def okValidation(uniqueId: String, errMsg: String): String =
    """
      |function validateOk%s() {
      |    return true;
      |}""".format(uniqueId).stripMargin + jsErrMsg(uniqueId, errMsg)

  private def failValidation(uniqueId: String, errMsg: String): String =
    """
      |function validateFail%s() {
      |    return false;
      |}""".format(uniqueId).stripMargin + jsErrMsg(uniqueId, errMsg)

  private def regexValidation(fieldId: String, uniqueId: String, regex: String, errMsg: String, shouldMatch: Boolean): String = {
    """
      |function validateRegex%s() {
      |    var x = document.getElementById("%s");
      |    var re = /%s/;
      |    if (re.test(x.value))
      |        return %s;
      |    else
      |        return %s;
      |}""".format(uniqueId, fieldId, regex, shouldMatch, !shouldMatch).stripMargin + jsErrMsg(uniqueId, errMsg)
  }

  private def requiredValidation(fieldId: String, uniqueId: String, errMsg: String): String = {
    """
      |function validateRequired%s() {
      |    var x = document.getElementById("%s");
      |    return x.value;
      |}""".format(uniqueId, fieldId).stripMargin + jsErrMsg(uniqueId, errMsg)
  }

  private def radioRequiredValidation(fieldId: String, groupId: String, uniqueId: String, errMsg: String): String = {
    """
      |function validateRadio%s() {
      |    var checked = false;
      |    var radios = document.getElementsByName("%s");
      |    for (var i = 0, radio; radio = radios[i]; i++) {
      |        if (radio.checked) {
      |            checked = true;
      |            break;
      |        }
      |    }
      |    return checked;
      |}""".format(fieldId, groupId).stripMargin + jsErrMsg(uniqueId, errMsg)
  }


  private def errHelperIfLabelAfter(field: Field): String = {
    if (HtmlBuilder.styleContainsLabelAfter(field.style.getOrElse(EmptyStyle)))
      """
        |window.onload = function() {
        |var btn=document.createElement("errMsg");
        |btn.setAttribute("id","errMsg%s");
        |var t=document.createTextNode(" ");
        |btn.appendChild(t);
        |document.getElementById("label%s").appendChild(btn);
        |}
      """.format(field.id, field.id).stripMargin
    else ""
  }
}