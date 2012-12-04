package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object JavaScriptBuilder {
  private def scriptTag(s: String) = {
    "\n" +
      "<script>" + "\n" +
      s + "\n" +
      "</script>"
  }

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
    """.stripMargin

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

  private def emailValidation: String =
    """
      |function validateEmail(name) {
      |    var x = document.getElementById(name);
      |    var re = /^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$/;
      |    if (re.test(x.value))
      |        x.setAttribute("validField", "valid");
      |    else
      |        x.setAttribute("validField", "notValid");
      |}
    """.stripMargin + formValidationScript


  def testValidation: String = {
    scriptTag(emailValidation)
  }

  def okValidation: String =
    """
      |function validateOk() {
      |    return true;
      |}
    """.stripMargin

  def failValidation: String =
    """
      |function validateFail() {
      |    return false;
      |}
    """.stripMargin

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

  def getJSFunctionName(js: String) = js.substring(js.indexOf("function") + 8, js.indexOf(")") + 1).trim

  def validationScriptForForm(form: Form): String = {
    def fieldsHelper(fields: List[Field]): List[String] = fields match {
      case Nil         => Nil
      case field :: xs => field.rule match {
        case Some(rule) =>
          val validationFunctions = ruleToFunctionStrings(rule, form)
          val functionNames = validationFunctions.map(f => getJSFunctionName(f))
          validationFunctions.mkString("") + fieldValidation(functionNames, field.id).mkString("") :: fieldsHelper(xs)
        case None       => fieldsHelper(xs)
      }
    }
    scriptTag(fieldsHelper(form.fields.toList).mkString("") + formValidationScript)
  }
}