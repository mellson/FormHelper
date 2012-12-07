package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object JavaScriptBuilder {

  // idGenerator used to have a unique id on each rule to be evaluated
  object idGenerator {
    var id: Long = 0

    def nextId = {
      id += 1
      id
    }
  }

  // Returns a tuple with the validation function and a unique id for each rule in a rule list
  private def ruleToFunctionTuples(rule: Rule, field: Field, form: Form, customErr: String = "", counter: Int = 0): List[(String, String)] = {
    val id = idGenerator.nextId
    rule match {
      case AndRule(r1, r2) => ruleToFunctionTuples(r1, field, form) ++ ruleToFunctionTuples(r2, field, form)
      case ErrorRule(r, error) => ruleToFunctionTuples(r, field, form, error)
      case OK(error) => (okValidation(field.id + id, if (customErr.isEmpty) error else customErr), field.id + id) :: Nil
      case FAIL(error) => (failValidation(field.id + id, if (customErr.isEmpty) error else customErr), field.id + id) :: Nil
      case Required(error) => (requiredValidation(field.id, field.id + id, if (customErr.isEmpty) error else customErr), field.id + id) :: Nil
      case MatchRegex(regex) => (regexValidation(field.id, field.id + id, regex, if (customErr.isEmpty) rule.error else customErr, shouldMatch = true), field.id + id) :: Nil
      case DoNotMatchRegex(regex) => (regexValidation(field.id, field.id + id, regex, if (customErr.isEmpty) rule.error else customErr, shouldMatch = false), field.id + id) :: Nil
      case <(e1, e2) => ("error", field.id + id) :: Nil
      case <=(e1, e2) => ("error", field.id + id) :: Nil
      case >(e1, e2) => ("error", field.id + id) :: Nil
      case >=(e1, e2) => ("error", field.id + id) :: Nil
      case ===(e1, e2) => ("error", field.id + id) :: Nil
      case !==(e1, e2) => ("error", field.id + id) :: Nil
    }
  }

  // Extract the function name of a function
  private def functionNameExtractor(js: String) = js.substring(js.indexOf("function") + 8, js.indexOf(")") + 1).trim

  // Returns JavaScript for validating a form
  def validationScriptForForm(form: Form): String = {
    def fieldsHelper(fields: List[Field]): List[String] = fields match {
      case Nil => Nil
      case field :: xs => field.rule match {
        case Some(rule) =>
          val ruleFunctionTuples = ruleToFunctionTuples(rule, field, form)
          val functions = ruleFunctionTuples.map(functionAndUniqueId => functionAndUniqueId._1)
          val functionNamesAndUniqueIds = ruleFunctionTuples.map(functionAndUniqueId => (functionNameExtractor(functionAndUniqueId._1), functionAndUniqueId._2))
          functions.mkString("") + fieldValidation(functionNamesAndUniqueIds, field.id).mkString("") :: fieldsHelper(xs)
        case None => fieldsHelper(xs)
      }
    }
    "\n<script>" + fieldsHelper(form.fields.toList).mkString("") + formValidationScript(form) + "\n</script>"
  }

  private def filterSubmit(field: Field) = field match {
    case Submit(_, _, _, _) => false
    case _ => true
  }

  private def formValidationScript(form: Form): String =
    """
      |function validateForm() {
      | """.stripMargin + // Validate each field when a user tries to submit the form
      (for {field <- form.fields.filter(f => filterSubmit(f))} yield "   validate%s();".format(field.id)).mkString("\n") +
      """
        |    var formContainer = document.getElementById("%s");
        |    var inputElements = formContainer.getElementsByTagName("input");
        |    console.log(inputElements);
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
        |    }
        |    return true;
        |}""".format(form.id).stripMargin

  private def functionValidationHelper(functionNameAndUniqueId: (String, String)): String =
    """
    if (!%s) {
        x.setAttribute("validField", "notValid");
        xErrField.innerHTML = %serrMsg;
        return false;
    }""".format(functionNameAndUniqueId._1, functionNameAndUniqueId._2)

  private def fieldValidation(functionNames: Seq[(String, String)], fieldId: String): String =
    """
      |function validate%s() {
      |    var xErrField = document.getElementById("errMsg%s");
      |    var x = document.getElementById("%s");""".format(fieldId, fieldId, fieldId).stripMargin +
      (for {fn <- functionNames} yield functionValidationHelper(fn)).mkString("") +
      """
        |    x.setAttribute("validField", "valid");
        |    xErrField.innerHTML = "";
        |    return true;
        |}
        | """.stripMargin

  // Create a JavaScript variable containing the error message
  def jsErrMsg(fieldId: String, errMsg: String) = "\nvar " + fieldId + "errMsg = '" + errMsg + "'\n"

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

  def regexValidation(fieldId: String, uniqueId: String, regex: String, errMsg: String, shouldMatch: Boolean): String = {
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

  def requiredValidation(fieldId: String, uniqueId: String, errMsg: String): String = {
    """
      |function validateRequired%s() {
      |    var x = document.getElementById("%s");
      |    return x.value;
      |}
    """.format(uniqueId, fieldId).stripMargin + jsErrMsg(uniqueId, errMsg)
  }
}