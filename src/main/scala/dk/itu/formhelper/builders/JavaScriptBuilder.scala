package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object JavaScriptBuilder {

  // idGenerator used to have a unique id on each rule to be evaluated
  object idGenerator {
    var id: Long = -1

    def nextId = {
      id += 1
      id
    }
  }

  // Returns a tuple with the validation function and a unique id for each rule in a rule list
  private def ruleToFunctions(rule: Rule, field: Field, form: Form, customErr: String = "", fieldRefID: String = ""): List[(String, String, String)] = {
    lazy val id = idGenerator.nextId
    lazy val fieldID = if (fieldRefID.isEmpty) field.id else fieldRefID
    lazy val uniqueID = field.id + id
    lazy val errMsg = if (customErr.isEmpty) rule.error else customErr

    field match {
      case Submit(_, _, _, _)                       => Nil
      case Radio(_, _, _, _) | Checkbox(_, _, _, _) =>
        if (ruleList(rule).contains(Required)) (groupRequiredValidation(fieldID, field.name, uniqueID, errMsg), uniqueID, fieldRefID) :: Nil
        else Nil
      case _                                        => rule match {
        case AndRule(r1, r2)                         => ruleToFunctions(r1, field, form, customErr, fieldRefID) ++ ruleToFunctions(r2, field, form, customErr, fieldRefID)
        case ErrorRule(r, error)                     => ruleToFunctions(r, field, form, error, fieldRefID)
        case ShowWhen(ref, r, error)                 => ruleToFunctions(r, field, form, error, ref.id)
        case OK(error)                               => (okValidation(uniqueID, if (customErr.isEmpty) error else customErr), uniqueID, fieldRefID) :: Nil
        case FAIL(error)                             => (failValidation(uniqueID, if (customErr.isEmpty) error else customErr), uniqueID, fieldRefID) :: Nil
        case Required(error)                         => (requiredValidation(fieldID, field.id + id, if (customErr.isEmpty) error else customErr), field.id + id, fieldRefID) :: Nil
        case MatchRegex(regex)                       => (regexValidation(fieldID, uniqueID, regex, errMsg, shouldMatch = true), field.id + id, fieldRefID) :: Nil
        case DoNotMatchRegex(regex)                  => (regexValidation(fieldID, uniqueID, regex, errMsg, shouldMatch = false), field.id + id, fieldRefID) :: Nil
        case <(Length(ThisField), Const(n: Int))     => (lengthValidation(fieldID, uniqueID, n, "<", errMsg), uniqueID, fieldRefID) :: Nil
        case <=(Length(ThisField), Const(n: Int))    => (lengthValidation(fieldID, uniqueID, n, "<=", errMsg), uniqueID, fieldRefID) :: Nil
        case >(Length(ThisField), Const(n: Int))     => (lengthValidation(fieldID, uniqueID, n, ">", errMsg), uniqueID, fieldRefID) :: Nil
        case >=(Length(ThisField), Const(n: Int))    => (lengthValidation(fieldID, uniqueID, n, ">=", errMsg), uniqueID, fieldRefID) :: Nil
        case ===(Length(ThisField), Const(n: Int))   => (lengthValidation(fieldID, uniqueID, n, "==", errMsg), uniqueID, fieldRefID) :: Nil
        case <(Length(ThisField), Length(ref))       => (lengthRefValidation(fieldID, uniqueID, ref, "<", errMsg), uniqueID, fieldRefID) :: Nil
        case <=(Length(ThisField), Length(ref))      => (lengthRefValidation(fieldID, uniqueID, ref, "<=", errMsg), uniqueID, fieldRefID) :: Nil
        case >(Length(ThisField), Length(ref))       => (lengthRefValidation(fieldID, uniqueID, ref, ">", errMsg), uniqueID, fieldRefID) :: Nil
        case >=(Length(ThisField), Length(ref))      => (lengthRefValidation(fieldID, uniqueID, ref, ">=", errMsg), uniqueID, fieldRefID) :: Nil
        case ===(Length(ThisField), Length(ref))     => (lengthRefValidation(fieldID, uniqueID, ref, "==", errMsg), uniqueID, fieldRefID) :: Nil
        case <(Value(ThisField), Const(n: Int))      => (intValueValidation(fieldID, uniqueID, n, "<", errMsg), uniqueID, fieldRefID) :: Nil
        case <=(Value(ThisField), Const(n: Int))     => (intValueValidation(fieldID, uniqueID, n, "<=", errMsg), uniqueID, fieldRefID) :: Nil
        case >(Value(ThisField), Const(n: Int))      => (intValueValidation(fieldID, uniqueID, n, ">", errMsg), uniqueID, fieldRefID) :: Nil
        case >=(Value(ThisField), Const(n: Int))     => (intValueValidation(fieldID, uniqueID, n, ">=", errMsg), uniqueID, fieldRefID) :: Nil
        case ===(Value(ThisField), Const(n: Int))    => (intValueValidation(fieldID, uniqueID, n, "==", errMsg), uniqueID, fieldRefID) :: Nil
        case <(Value(ThisField), Const(n: Double))   => (doubleValueValidation(fieldID, uniqueID, n, "<", errMsg), uniqueID, fieldRefID) :: Nil
        case <=(Value(ThisField), Const(n: Double))  => (doubleValueValidation(fieldID, uniqueID, n, "<=", errMsg), uniqueID, fieldRefID) :: Nil
        case >(Value(ThisField), Const(n: Double))   => (doubleValueValidation(fieldID, uniqueID, n, ">", errMsg), uniqueID, fieldRefID) :: Nil
        case >=(Value(ThisField), Const(n: Double))  => (doubleValueValidation(fieldID, uniqueID, n, ">=", errMsg), uniqueID, fieldRefID) :: Nil
        case ===(Value(ThisField), Const(n: Double)) => (doubleValueValidation(fieldID, uniqueID, n, "==", errMsg), uniqueID, fieldRefID) :: Nil
        case <(Value(ThisField), Const(n: String))   => (stringValueValidation(fieldID, uniqueID, n, "<", errMsg), uniqueID, fieldRefID) :: Nil
        case <=(Value(ThisField), Const(n: String))  => (stringValueValidation(fieldID, uniqueID, n, "<=", errMsg), uniqueID, fieldRefID) :: Nil
        case >(Value(ThisField), Const(n: String))   => (stringValueValidation(fieldID, uniqueID, n, ">", errMsg), uniqueID, fieldRefID) :: Nil
        case >=(Value(ThisField), Const(n: String))  => (stringValueValidation(fieldID, uniqueID, n, ">=", errMsg), uniqueID, fieldRefID) :: Nil
        case ===(Value(ThisField), Const(n: String)) => (stringValueValidation(fieldID, uniqueID, n, "==", errMsg), uniqueID, fieldRefID) :: Nil
        case <(Value(ThisField), Value(ref))         => (valueRefValidation(fieldID, uniqueID, ref, "<", errMsg), uniqueID, fieldRefID) :: Nil
        case <=(Value(ThisField), Value(ref))        => (valueRefValidation(fieldID, uniqueID, ref, "<=", errMsg), uniqueID, fieldRefID) :: Nil
        case >(Value(ThisField), Value(ref))         => (valueRefValidation(fieldID, uniqueID, ref, ">", errMsg), uniqueID, fieldRefID) :: Nil
        case ===(Value(ThisField), Value(ref))       => (valueRefValidation(fieldID, uniqueID, ref, "==", errMsg), uniqueID, fieldRefID) :: Nil
        case >=(Value(ThisField), Value(ref))        => (valueRefValidation(fieldID, uniqueID, ref, ">=", errMsg), uniqueID, fieldRefID) :: Nil
      }
    }
  }

  // Returns JavaScript for validating a form
  def validationScriptForForm(form: Form): String = {
    // Extract the function name of a function
    def functionNameExtractor(js: String) = js.substring(js.indexOf("function") + 8, js.indexOf(")") + 1).trim

    def fieldsHelper(fields: List[Field]): List[String] = fields match {
      case Nil         => Nil
      case field :: xs => field.rule match {
        case Some(rule) =>
          val ruleFunctionTuples = ruleToFunctions(rule, field, form)
          val functions = ruleFunctionTuples.map(functionAndUniqueIdAndRef => functionAndUniqueIdAndRef._1)
          val functionNamesAndUniqueIdsAndRef = ruleFunctionTuples.map(functionAndUniqueIdAndRef => (functionNameExtractor(functionAndUniqueIdAndRef._1), functionAndUniqueIdAndRef._2, functionAndUniqueIdAndRef._3))
          functions.mkString + fieldValidation(functionNamesAndUniqueIdsAndRef, field).mkString + errHelperIfLabelAfter(field) :: fieldsHelper(xs)
        case None       => fieldsHelper(xs)
      }
    }
    "\n<script>" + fieldsHelper(form.fields.toList).mkString + formValidationScript(form) + "\n</script>"
  }

  // Returns JavaScript that will validate the supplied form
  private def formValidationScript(form: Form): String = {
    val fieldFocusList = (for {field <- form.fields} yield """document.getElementById("%s").focus();""".format(field.id)).mkString("\n    ")
    val result = """
                   |function validateForm() {
                   |    %s
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
                   |}""".format(fieldFocusList, form.id).stripMargin
    result
  }


  // Decides if the field should use normal validation or ref or a mix
  private def fieldValidation(functionNames: Seq[(String, String, String)], field: Field): String = {
    val valueFunctionTuples = for {
      fn <- functionNames
      if (fn._3.isEmpty) // Check if there is a reference to another field. If not create a function that validates this field
    } yield (fn._1, fn._2)

    val refFunctionTuples = for {
      fn <- functionNames
      if (!fn._3.isEmpty)
    } yield fn

    val valueFunctionString = if (!valueFunctionTuples.isEmpty) fieldValueValidation(valueFunctionTuples, field) else ""
    val refFunctionString = if (!refFunctionTuples.isEmpty) fieldRefValidation(refFunctionTuples, field) else ""
    valueFunctionString + refFunctionString
  }

  private def idHelper(field: Field) = field match {
    case Radio(_, _, _, _) | Checkbox(_, _, _, _) => field.name
    case _                                        => field.id
  }

  // Validation function for fields that have dependencies
  private def fieldRefValidation(functionNames: Seq[(String, String, String)], field: Field): String = {
    def functionValidationHelper(functionNameAndUniqueId: (String, String, String)): String = {
      """
        |    if (!%s) {
        |    var field = document.getElementById("%s");
        |    field.setAttribute("type","hidden");
        |    return false;
    }""".format(functionNameAndUniqueId._1, field.id).stripMargin
    }

    val valFuncName = "validateRef" + idHelper(field)
    val fieldRefName = functionNames.head._3
    val onLoadFunction = """
                           |window.onload = function() {
                           |    var x = document.getElementById("%s");
                           |    x.setAttribute("onkeyup","%s()");
                           |}""".format(fieldRefName, valFuncName).stripMargin + "\n"

    val result = """
                   |function validateRef%s() {""".format(idHelper(field)).stripMargin +
      (for {fn <- functionNames} yield functionValidationHelper(fn)).mkString +
      """
        |    var field = document.getElementById("%s");
        |    var type = field.getAttribute("originaltype");
        |    field.setAttribute("type",type);
        |}
      """.format(field.id).stripMargin

    onLoadFunction + result
  }

  // Validation function for a field. Checks each validation function for a field and sets the attribute validField accordingly
  private def fieldValueValidation(functionNames: Seq[(String, String)], field: Field): String = {
    def functionValidationHelper(functionNameAndUniqueId: (String, String)): String =
      """
    if (!%s) {
        x.setAttribute("validField", "notValid");
        xErrField.innerHTML = %serrMsg;
        return false;
    }""".format(functionNameAndUniqueId._1, functionNameAndUniqueId._2)

    val result = """
                   |function validate%s() {
                   |    var xErrField = document.getElementById("errMsg%s");
                   |    var x = document.getElementById("%s");""".format(idHelper(field), field.id, field.id).stripMargin +
      (for {fn <- functionNames} yield functionValidationHelper(fn)).mkString +
      """
        |    x.setAttribute("validField", "valid");
        |    xErrField.innerHTML = "";
        |    return true;
        |}""".stripMargin
    result
  }

  // Create a JavaScript variable containing the error message
  def jsErrMsg(fieldId: String, errMsg: String) = "\nvar " + fieldId + "errMsg = '" + errMsg + "'\n"

  // Validation function for Length
  private def lengthValidation(fieldId: String, uniqueId: String, length: Int, operator: String, errMsg: String) =
    """
      |function validateLength%s() {
      |    var x = document.getElementById("%s");
      |    return x.value.length %s %d;
      |}""".format(uniqueId, fieldId, operator, length).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Validation function for Int Value
  private def intValueValidation(fieldId: String, uniqueId: String, value: Int, operator: String, errMsg: String) =
    """
      |function validateIntValue%s() {
      |    var x = document.getElementById("%s");
      |    return x.value %s %d;
      |}""".format(uniqueId, fieldId, operator, value).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Validation function for Double Value
  private def doubleValueValidation(fieldId: String, uniqueId: String, value: Double, operator: String, errMsg: String) =
    """
      |function validateDoubleValue%s() {
      |    var x = document.getElementById("%s");
      |    return x.value %s %f;
      |}""".format(uniqueId, fieldId, operator, value).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Validation function for String Value
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

  // Validation function for Length references
  private def lengthRefValidation(fieldId: String, uniqueId: String, ref: FieldRef, operator: String, errMsg: String) =
    """
      |function validateRefLength%s() {
      |    var x = document.getElementById("%s");
      |    var y = document.getElementById("%s");
      |    return x.value.length %s y.value.length;
      |}""".format(uniqueId, fieldId, jsRefHelper(ref, fieldId), operator).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Validation function for Value references
  private def valueRefValidation(fieldId: String, uniqueId: String, ref: FieldRef, operator: String, errMsg: String) =
    """
      |function validateRefValue%s() {
      |    var x = document.getElementById("%s");
      |    var y = document.getElementById("%s");
      |    return x.value %s y.value;
      |}""".format(uniqueId, fieldId, jsRefHelper(ref, fieldId), operator).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Validation function for the OK rule
  private def okValidation(uniqueId: String, errMsg: String): String =
    """
      |function validateOk%s() {
      |    return true;
      |}""".format(uniqueId).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Validation function for the FAIL rule
  private def failValidation(uniqueId: String, errMsg: String): String =
    """
      |function validateFail%s() {
      |    return false;
      |}""".format(uniqueId).stripMargin + jsErrMsg(uniqueId, errMsg)

  // Validation function for Regex
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

  // Validation function for the required rule
  private def requiredValidation(fieldId: String, uniqueId: String, errMsg: String): String = {
    """
      |function validateRequired%s() {
      |    var x = document.getElementById("%s");
      |    return x.value;
      |}""".format(uniqueId, fieldId).stripMargin + jsErrMsg(uniqueId, errMsg)
  }

  // Validation function for groups. Groups are radio or checkbox.
  private def groupRequiredValidation(fieldId: String, groupId: String, uniqueId: String, errMsg: String): String = {
    """
      |function validateGroup%s() {
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

  // Creates an element after the Label for showing a fields error message
  private def errHelperIfLabelAfter(field: Field): String = {
    if (HtmlBuilder.fieldContainsLabelAfter(field))
      """
        |window.onload = function() {
        |    var x = document.createElement("errMsg");
        |    x.setAttribute("id","errMsg%s");
        |    var t=document.createTextNode(" ");
        |    btn.appendChild(t);
        |    document.getElementById("label%s").appendChild(btn);
        |}""".format(field.id, field.id).stripMargin
    else ""
  }
}