package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

object HtmlBuilder {
  private val indent = "  "

  def formHtml(form: Form, validate: Boolean): String = {
    // Check if style1 contains style2 
    def styleMatcher(style1: Style, style2: Style): Boolean = style1 match {
      case AndStyle(s1, s2) => styleMatcher(s1, style2) || styleMatcher(s2, style2)
      case s                => s == style2
    }

    // Places <br> after each field except the last field
    def brHelper(field: Field): String = if (styleMatcher(field.style.getOrElse(EmptyStyle), SameLine)) "" else "<br>"

    // Build HTML for form, add newline after each field except the last
    def newLineHelper(fields: List[Field], html: String): String = fields match {
      case Nil      => html + "</form>"
      case f :: Nil => newLineHelper(Nil, html + indent + fieldHtml(f, validate) + "\n")
      case f :: fs  => newLineHelper(fs, html + indent + fieldHtml(f, validate) + brHelper(f) + "\n")
    }

    val validation = if (validate) """onsubmit="return validateForm()"""" else ""
    newLineHelper(form.fields.toList, """<form name="%s" method="%s" id="%s" action="%s" %s>""".format(form.name, form.method.toString.toLowerCase, form.id, form.action, validation) + "\n")
  }

  // Does the field contain a Label placed after the field?
  def fieldContainsLabelAfter(field: Field) = {
    def helper(sl: List[Style]): Boolean = sl match {
      case Nil                   => false
      case Label(_, After) :: xs => true
      case _ :: xs               => helper(xs)
    }
    helper(styleList(field.style.getOrElse(EmptyStyle)))
  }

  def fieldHtml(field: Field, validate: Boolean): String = {
    // Check if the field depends on another field.
    def dependsOnOtherField(field: Field): Boolean = !ruleList(field.rule.getOrElse(EmptyRule)).filter({
      case ShowWhen(_, _, _) => true
      case _                 => false
    }).isEmpty

    // Checks if the field is only depending on another field. Eg doesn't have it's own requirements that needs to be validated.
    def onlyDependsOnOtherField(field: Field): Boolean = ruleList(field.rule.getOrElse(EmptyRule)) match {
      case ShowWhen(_, _, _) :: Nil => true
      case _                        => false
    }

    def htmlStartHelper(field: Field): String = {
      if (dependsOnOtherField(field) && field.value.isEmpty) """<input type="hidden" originaltype="%s" name="%s" id="%s"""".format(field.inputType, field.name, field.id) + valueHelper(field.value)
      else """<input type="%s" name="%s" id="%s"""".format(field.inputType, field.name, field.id) + valueHelper(field.value)
    }

    // Adds validation method to a Field
    def validationHelper(field: Field, validate: Boolean): String = field match {
      case Submit(_, _, _, _) => ""
      case _                  => if (validate && field.rule != None && !onlyDependsOnOtherField(field)) """ onblur="validate%s()" validField=""""".format(field.name) else ""
    }

    // Value string helper
    def valueHelper(value: String): String = if (value.isEmpty) "" else """ value="%s"""".format(value)

    // If we are creating Html with validation, create a placeholder for JavaScript to output the error message to
    lazy val validationErrorPlaceholder = if (validate && field.rule != None && !fieldContainsLabelAfter(field)) """<errMsg id="errMsg""" + field.id + """"></errMsg>""" else ""

    val htmlStart = htmlStartHelper(field)
    val htmlEnd = validationHelper(field, validate) + ">" + validationErrorPlaceholder

    lazy val fieldInfoStart = "\n" + indent + indent + "<field_message class=\"info\">"
    lazy val fieldErrorStart = "\n" + indent + indent + "<field_message class=\"error\">"
    val fieldEnd = "</field_message>"

    def labelHelper(label: String): String = """<labelElem id="label%s">%s </labelElem>""".format(field.id, label)

    def styleHelper(styles: List[Style], html: String): String = styles match {
      case Label(label, placement) :: xs => placement match {
        case Before => labelHelper(label) + styleHelper(xs, html)
        // Wrapping a label placed after the field in a labelElement so that JavaScript has a place to write error messages
        case After => styleHelper(xs, html) + labelHelper(label)
        // HTML 5 feature
        case Inside => styleHelper(xs, html + " placeholder=\"" + label + "\"")
      }
      // Radio button or Checkbox checked
      case Checked :: xs => styleHelper(xs, html + " checked")

      case ShowErrors(err) :: xs   => styleHelper(xs, html + " validField=\"notValid\"") + fieldErrorStart + err + fieldEnd
      case ShowRequirements :: xs => styleHelper(xs, html) + fieldInfoStart + field.rule.getOrElse(EmptyRule).error + fieldEnd

      // A field without styles
      case x :: xs => styleHelper(xs, html)

      case Nil => htmlStart + html + htmlEnd
    }

    styleHelper(styleList(field.style.getOrElse(EmptyStyle)).sorted, "")
  }
}