package dk.itu.formhelper

import dk.itu.formhelper.Validator._
import dk.itu.formhelper.builders._

object FormHelper extends Styles with Rules with HtmlBuilder {
  final case class Form(name: String, method: Method, action: String, fields: Field*) {
    val html: String = formHtml(this, false)
    val htmlWithValidation: String = formHtml(this, true)
    
    def validatedForm(postData: Option[Map[String, Seq[String]]]) : (Boolean, Form)= {
      val postForm = ScalaBuilder.formFromPost(this, postData)
      val boolean = ScalaBuilder.validateForm(postForm)
      (boolean,postForm)
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
    val html: String = fieldHtml(this, false)
    val htmlWithValidation: String = fieldHtml(this, true)
    val rule: Rule
    val style: Style
  }

  private def addRule(newRule: Rule, existingRule: Rule): Rule = {
    if (existingRule == null) newRule
    else AndRule(existingRule, newRule)
  }

  private def addStyle(newStyle: Style, existingStyle: Style): Style = {
    if (existingStyle == null) newStyle
    else AndStyle(existingStyle, newStyle)
  }

  case class Text(name: String, value: String = "", rule: Rule = null, style: Style = null) extends Field {
    def id = name
    def inputType = "text"
    def withRule(r: Rule): Text = Text(name, value, addRule(r, rule), style)
    def withStyle(s: Style): Text = Text(name, value, rule, addStyle(s, style))
    def setValue(v: String): Text = Text(name, v, rule, style)
  }

  case class Submit(name: String, value: String = "", rule: Rule = null, style: Style = null) extends Field {
    def id = name
    def inputType = "submit"
    def withRule(r: Rule): Submit = this
    def withStyle(s: Style): Submit = Submit(name, value, rule, addStyle(s, style))
    def setValue(v: String) = this
  }

  case class Password(name: String, value: String = "", rule: Rule = null, style: Style = null) extends Field {
    def id = name
    def inputType = "password"
    def withRule(r: Rule): Password = Password(name, value, addRule(r, rule), style)
    def withStyle(s: Style): Password = Password(name, value, rule, addStyle(s, style))
    def setValue(v: String): Password = Password(name, v, rule, style)
  }

  case class Radio(name: String, value: String, rule: Rule = null, style: Style = null) extends Field {
    def id = name + value
    def inputType = "radio"
    def withRule(r: Rule): Radio = Radio(name, value, addRule(r, rule), style)
    def withStyle(s: Style): Radio = Radio(name, value, rule, addStyle(s, style))
    def setValue(v: String): Radio = Radio(name, value, rule, addStyle(Checked, style))
    def setChecked(b: Boolean) = if (b) Radio(name, value, rule, addStyle(Checked, style)) else Radio(name, value, rule, style)
  }

  sealed abstract class FieldRef {
    def name: String
  }
  case object ThisField extends FieldRef {
    def name = "this field"
  }
  case class FieldId(id: String) extends FieldRef {
    def name = "field " + id // TODO place this in a context where form is in scope so that the name can be accessed
  }
}

