package dk.itu.formhelper

import dk.itu.formhelper.builders._

object FormHelper extends Styles with Rules {

  final case class Form(name: String, method: Method, action: String, fields: Field*) {
    lazy val html: String = HtmlBuilder.formHtml(this, validate = false)
    lazy val htmlWithValidation: String = HtmlBuilder.formHtml(this, validate = true) + JavaScriptBuilder.validationScriptForForm(this)
    lazy val id: String = name.replaceAll(" ", "").toLowerCase

    def validatedForm(postData: Option[Map[String, Seq[String]]]): (Boolean, Form) = {
      val postForm = ScalaBuilder.formFromPost(this, postData)
      val boolean = ScalaBuilder.validateForm(postForm)
      (boolean, postForm)
    }
  }

  sealed abstract class Method

  case object Get extends Method

  case object Post extends Method

  sealed abstract class Field {
    def name: String

    def value: String

    def id: String

    def inputType: String

    def setValue(s: String): Field

    def withStyle(s: Style): Field

    def withRule(r: Rule): Field

    lazy val html: String = HtmlBuilder.fieldHtml(this, validate = false)
    lazy val htmlWithValidation: String = HtmlBuilder.fieldHtml(this, validate = true)
    val rule: Option[Rule]
    val style: Option[Style]
  }

  private def addRule(newRule: Rule, existingRule: Option[Rule]): Rule = {
    if (existingRule == None) newRule
    else AndRule(existingRule.get, newRule)
  }

  private def addStyle(newStyle: Style, existingStyle: Option[Style]): Style = {
    if (existingStyle == None) newStyle
    else AndStyle(existingStyle.get, newStyle)
  }

  case class Text(name: String, value: String = "", rule: Option[Rule] = None, style: Option[Style] = None) extends Field {
    def id = name

    def inputType = "text"

    def withRule(r: Rule): Text = Text(name, value, Some(addRule(r, rule)), style)

    def withStyle(s: Style): Text = Text(name, value, rule, Some(addStyle(s, style)))

    def setValue(v: String): Text = Text(name, v, rule, style)
  }

  case class Submit(name: String, value: String = "", rule: Option[Rule] = None, style: Option[Style] = None) extends Field {
    def id = name

    def inputType = "submit"

    def withRule(r: Rule): Submit = this

    def withStyle(s: Style): Submit = Submit(name, value, rule, Some(addStyle(s, style)))

    def setValue(v: String) = this
  }

  case class Password(name: String, value: String = "", rule: Option[Rule] = None, style: Option[Style] = None) extends Field {
    def id = name

    def inputType = "password"

    def withRule(r: Rule): Password = Password(name, value, Some(addRule(r, rule)), style)

    def withStyle(s: Style): Password = Password(name, value, rule, Some(addStyle(s, style)))

    def setValue(v: String): Password = Password(name, v, rule, style)
  }

  case class Radio(name: String, value: String, rule: Option[Rule] = None, style: Option[Style] = None) extends Field {
    def id = name + value

    def inputType = "radio"

    def withRule(r: Rule): Radio = Radio(name, value, Some(addRule(r, rule)), style)

    def withStyle(s: Style): Radio = Radio(name, value, rule, Some(addStyle(s, style)))

    def setValue(v: String): Radio = Radio(name, value, rule, Some(addStyle(Checked, style)))

    def setChecked(b: Boolean) = if (b) Radio(name, value, rule, Some(addStyle(Checked, style))) else Radio(name, value, rule, style)
  }

  // Converts a style to a list of styles
  def styleList(style: Style): List[Style] = style match {
    case AndStyle(s1, s2) => styleList(s1) ++ styleList(s2)
    case EmptyStyle => Nil
    case s => List(s)
  }

  // Converts a rule to a list of rules
  def ruleList(rule: Rule): List[Rule] = rule match {
    case AndRule(r1, r2) => ruleList(r1) ++ ruleList(r2)
    case EmptyRule => Nil
    case r => List(r)
  }
}

