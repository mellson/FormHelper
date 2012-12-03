package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object JavaScriptBuilder {
  private def scriptTag(s: String) = {
    "\n" +
      "<script>" +
      s +
      "</script>"
  }

  private def emailValidation: String = """
    function validateemail(name) {
        var x=document.getElementById(name);
        x.value=x.value.toUpperCase();
		document.write('Hello World!');
    }
                                        """

  def testValidation: String = {
    scriptTag(emailValidation)
  }

  private def ruleToFunction(rule: Rule, field: Field): String = rule match {
    case OK(error)             => ""
    case FAIL(error)           => ""
    case Required(error)       => ""
    case ErrorRule(r, error)   => ""
    case AndRule(r1, r2)       => ""
    case MatchRegex(regex)     => ""
    case DoNotMatchRegex(regex) => ""
    case <(e1, e2)             => ""
    case <=(e1, e2)            => ""
    case >(e1, e2)             => ""
    case >=(e1, e2)            => ""
    case ===(e1, e2)           => ""
    case !==(e1, e2)           => ""
  }

  def validationForField(field: Field): String = {
    ""
  }

}