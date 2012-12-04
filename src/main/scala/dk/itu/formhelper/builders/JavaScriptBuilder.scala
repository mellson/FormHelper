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
    """.stripMargin+ formValidationScript


  def testValidation: String = {
    scriptTag(emailValidation)
  }

  private def ruleToFunctionStrings(rule: Rule, form: Form): List[String] = rule match {
    case AndRule(r1, r2)        => ruleToFunctionStrings(r1, form) ++ ruleToFunctionStrings(r2, form)
    case OK(error)              => List(error)
    case FAIL(error)            => List(error)
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

  def validationScriptForForm(form: Form): String = {
    val rules: Seq[Rule] = for {
      field <- form.fields
      rule <- field.rule
    } yield rule

    val validationFunctions = for {
      rule <- rules
      functionString <- ruleToFunctionStrings(rule, form)
    } yield functionString
    scriptTag(validationFunctions.mkString("\n\n"))
  }

  //  def validationScriptForField(field: Field): String = {
  //    val functionStrings = for {
  //      rule1 <- field.rule
  //      rule2 <- ruleList(rule1)
  //    } yield ruleToFunctionStrings(rule2, field)
  //    scriptTag(functionStrings.mkString("\n"))
  //  }

}