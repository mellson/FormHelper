package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object JavaScriptBuilder {
  private def ruleToFunctionStrings(rule: Rule, form: Form): List[String] = rule match {
    case AndRule(r1, r2)        => ruleToFunctionStrings(r1, form) ++ ruleToFunctionStrings(r2, form)
    case OK(error)              => List(okValidation)
    case FAIL(error)            => List(failValidation)
    case Required(error)        => List(error)
    case ErrorRule(r, error)    => List(error)
    case MatchRegex(regex)      => List(regex)
    case DoNotMatchRegex(regex) => List(regex)
    case <(e1, e2)              => List("error")
    case <=(e1, e2)             => List("error")
    case >(e1, e2)              => List("error")
    case >=(e1, e2)             => List("error")
    case ===(e1, e2)            => List("error")
    case !==(e1, e2)            => List("error")
  }

  // Extract the function name of a function
  private def functionNameExtractor(js: String) = js.substring(js.indexOf("function") + 8, js.indexOf(")") + 1).trim

  // Returns JavaScript for validating a form
  def validationScriptForForm(form: Form): String = {
    def fieldsHelper(fields: List[Field]): List[String] = fields match {
      case Nil         => Nil
      case field :: xs => field.rule match {
        case Some(rule) =>
          val validationFunctions = ruleToFunctionStrings(rule, form)
          val functionNames = validationFunctions.map(function => functionNameExtractor(function))
          validationFunctions.mkString("") + fieldValidation(functionNames, field.id).mkString("") :: fieldsHelper(xs)
        case None       => fieldsHelper(xs)
      }
    }
    "<script>" + fieldsHelper(form.fields.toList).mkString("") + formValidationScript + "</script>"
  }

  /* Validation scripts as strings below here */

  private val formValidationScript: String =
    """
      |function validateForm() {
      |    var inputElements = document.getElementsByTagName("input");
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
      |}
      | """.stripMargin

  private def functionValidationHelper(functionName: String): String =
    """
      if (!""" +
      functionName +
      """) {
        |        x.setAttribute("validField", "notValid");
        |        return false;
        |    }
      """.stripMargin

  private def fieldValidation(functionNames: Seq[String], fieldId: String): String =
    """
      |function validate""".stripMargin + fieldId + """() {""".stripMargin +
      """
      var x = document.getElementById("""" + fieldId + """");""" +
      (for {fn <- functionNames} yield functionValidationHelper(fn)).mkString("") + """
                                                                                      |    x.setAttribute("validField", "valid");
                                                                                      |    return true;
                                                                                      |}
                                                                                    """.stripMargin

  private def okValidation: String =
    """
      |function validateOk() {
      |    return true;
      |}
    """.stripMargin

  private def failValidation: String =
    """
      |function validateFail() {
      |    return false;
      |}
    """.stripMargin
}