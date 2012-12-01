package dk.itu.formhelper.builders

import dk.itu.formhelper.FormHelper._

trait HtmlBuilder {
  private val indent = "  "
  def formHtml(form: Form, validate: Boolean): String = {
    // Check if style1 contains style2 
    def styleMatcher(style1: Style, style2: Style): Boolean = style1 match {
      case AndStyle(s1, s2) => styleMatcher(s1, style2) || styleMatcher(s2, style2)
      case s => s == style2
    }

    // Places <br> after each field except the last field
    def brHelper(field: Field): String = if (styleMatcher(field.style, SameLine)) "" else "<br>"

    def methodHelper(method: Method): String = method match {
      case Get => "method=\"get\""
      case Post => "method=\"post\""
    }

    // Build HTML for form, add newline after each field except the last
    def newLineHelper(fields: List[Field], html: String): String = fields match {
      case Nil => html + "</form>"
      case f :: Nil => newLineHelper(Nil, html + indent + fieldHtml(f, validate) + "\n")
      case f :: fs => newLineHelper(fs, html + indent + fieldHtml(f, validate) + brHelper(f) + "\n")
    }

    val validation = if (validate) " onsubmit=\"return validateForm()\"" else ""
    newLineHelper(form.fields.toList, "<form name=\"" + form.name + "\" " + methodHelper(form.method) + " action=\"" + form.action + "\"" + validation + ">\n")
  }

  def styleList(style: Style): List[Style] = style match {
    case AndStyle(s1,s2) => styleList(s1) ++ styleList(s2)
    case s => List(s)
  }
  
  def fieldHtml(field: Field, validate: Boolean): String = {
    // Adds validation method to a Field
    def validationHelper(field: Field, validate: Boolean): String = field match {
      case Submit(_, _, _, _) => ""
      case _ => if (validate) " onblur=\"validate" + field.name + "()\"" else ""
    }

    // Value string helper
    def valueHelper(value: String): String = if (value.isEmpty()) "" else "\"" + " value=\"" + value

    val htmlStart = "<input type=\"" + field.inputType + "\" name=\"" + field.name + valueHelper(field.value) + "\""
    val htmlEnd = validationHelper(field, validate) + ">"
    lazy val fieldInfoStart = "\n" + indent + indent + "<field_message class=\"info\">"
    lazy val fieldErrorStart = "\n" + indent + indent + "<field_message class=\"error\">"
    val fieldEnd = "</field_message>"

    def styleHelper(styles: List[Style], html: String): String = styles match {
      case Label(label, placement)::xs => placement match {
        case Before => label + styleHelper(xs, html)
        case After => styleHelper(xs, html) + label
        // HTML 5 feature
        case Inside => styleHelper(xs, html + " placeholder=\"" + label + "\"")
      }
      // Radio button checked
      case Checked::xs => styleHelper(xs, html + " checked")

      case ShowRequirements::xs => styleHelper(xs, html) + fieldInfoStart + field.rule.error + fieldEnd
      case ShowErrors::xs => styleHelper(xs, html) + fieldErrorStart + field.rule.error + fieldEnd
      
      // A field without styles
      case x::xs => styleHelper(xs, html)
      
      case Nil => htmlStart + html + htmlEnd //htmlStart + htmlEnd
    }
    styleHelper(styleList(field.style), "")
  }
}