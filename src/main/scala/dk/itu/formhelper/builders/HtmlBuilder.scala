package dk.itu.formhelper.builders
import dk.itu.formhelper.FormHelper._

object HtmlBuilder {
  private val indent = "  "

  private def valueHelper(value: String): String = if (value.isEmpty()) "" else  "\"" + " value=\"" + value
  private def validationHelper[T](field: Field[T], validate: Boolean): String = field match {
    case Submit(_,_,_,_) => ""
    case _ => if (validate) " onblur=\"validate" + field.fname + "()\"" else ""  
  }
  def htmlField[T](field: Field[T], validate: Boolean): String = field match {
    case Text(fname, value, styles, rules) => styleHelper("<input type=\"text\" id=\"" + field.id + "\" name=\"" + fname + valueHelper(value) + "\"" + validationHelper(field, validate), styles, rules)
    case Password(fname, value, styles, rules) => styleHelper("<input type=\"password\" id=\"" + field.id + "\" name=\"" + fname + valueHelper(value) + "\"" + validationHelper(field, validate), styles, rules)
    case Radio(fname, value, styles, rules) => styleHelper("<input type=\"radio\" id=\"" + field.id + "\" name=\"" + fname + valueHelper(value) + "\"" + validationHelper(field, validate), styles, rules)
    case Submit(fname, value, styles, rules) => styleHelper("<input type=\"submit\" id=\"" + field.id + "\" value=\"" + fname + valueHelper(value) + "\"" + validationHelper(field, validate), styles, rules)
  }  
  
  def plainHtml[T](form: Form[T]): String = htmlForm(form, false)
  
  def htmlWithValidation[T](form: Form[T]): String = htmlForm(form, true)
    
  // Create Html from a Form type
  private def htmlForm[T](form: Form[T], validate: Boolean): String = {

    // Places <br> after each field except the last field
    def brHelper(field: Field[T]): String = if (field.styles.contains(SameLine)) "" else "<br>"

    def methodHelper[T](method: Method): String = method match {
      case Get => "method=\"get\""
      case Post => "method=\"post\""
    }

    // Build HTML for form, add newline after each field except the last
    def helper(fields: List[Field[T]], html: String): String = fields match {
      case Nil => html + "</form>"
      case f :: Nil => helper(Nil, html + indent + htmlField(f, validate) + "\n")
      case f :: fs => helper(fs, html + indent + htmlField(f, validate) + brHelper(f) + "\n")
    }

    val validation = if (validate) " onsubmit=\"return validateForm()\"" else ""
    helper(form.fields.toList, "<form name=\"" + form.name + "\" " + methodHelper(form.method) + " action=\"" + form.action + "\"" + validation + ">\n")
  }

  // Build HTML for requirements
  private def reqHelper(rules: List[Rule], start: String, end: String): String = {
    def lengthHelper(rules2: List[Rule], vals: (Int, Int, Int)): Length = rules2 match {
      case Length(min1, max1, equals1) :: xs =>
        val min2 = min1 max vals._1
        val max2 = max1 max vals._2
        val equals2 = equals1 max vals._3
        lengthHelper(xs, (min2, max2, equals2))
      case _ :: xs => lengthHelper(xs, vals)
      case Nil => Length(vals._1, vals._2, vals._3)
    }

    def lengthString(length: Length): String = {
      val l_start = start + "Length must be "
      val l_end = " characters" + end
      length match {
        case Length(min, 0, 0) => l_start + "greater-than " + min + l_end
        case Length(0, max, 0) => l_start + "less-than " + max + l_end
        case Length(min, max, 0) => l_start + "greater-than " + min + " and less-than " + max + l_end
        case Length(min, max, equals) => 
          if (equals == min)
            if (max==0) l_start + "greater-than or equal to " + equals + l_end
            else l_start + "greater-than or equal to " + equals + " and less-than " + max + l_end
          else if (equals == max)
            if (min==0) l_start + "less-than or equal to " + equals + l_end
            else l_start + "greater-than " + min + " and less-than or equal to " + equals + l_end
          else l_start + equals + l_end
      }
    }
    
    def isLength(r: Rule) = r match {
      case Length(_,_,_) => true
      case _			 => false
    }

    rules match {
      case Required :: xs => start + "Field is required" + end + reqHelper(xs, start, end)
      case Length(min, max, equals) :: xs => lengthString(lengthHelper(xs, (min, max, equals))) + reqHelper(xs.filter(r=>(!isLength(r))), start, end)
      case Matches(field) :: xs => start + "Must match " + field + end + reqHelper(xs, start, end)
      case Email :: xs => start + "Must be a valid email" + end + reqHelper(xs, start, end)
      case IP :: xs => start + "Must be a valid IP" + end + reqHelper(xs, start, end)
      case Integer :: xs => start + "Must be an integer" + end + reqHelper(xs, start, end)
      case Float :: xs => start + "Must be a float" + end + reqHelper(xs, start, end)
      case Alpha :: xs => start + "Must be alphanumeric" + end + reqHelper(xs, start, end)
      case Regex(regex, bool) :: xs =>
        val not = if (bool) "" else "not"
        start + "Must " + not + " match this regex: " + regex + end + reqHelper(xs, start, end)
      case _ => "" 
    }
  }

  // Build HTML for an rule related type
  private def ruleHelper(errMsg: String, styles: List[Style], rules: List[Rule]): String = {
    val fieldInfoStart = "\n" + indent + indent + "<field_message class=\"info\">"
    val fieldErrorStart = "\n" + indent + indent + "<field_message class=\"error\">"
    val fieldEnd = "</field>"
    
    def containsError(ss: List[Style]): Boolean = ss match {
      case Error(msg)::xs => true
      case _::xs => containsError(xs)
      case Nil => false
    }
      
    if (styles.contains(ShowRequirements) && containsError(styles))
      if (errMsg.isEmpty()) "" else fieldInfoStart + errMsg + fieldEnd
    else if (styles.contains(ShowRequirements) && !containsError(styles))
      reqHelper(rules, fieldErrorStart, fieldEnd)
    else if (styles.contains(ShowErrors) && containsError(styles))
      if (errMsg.isEmpty()) "" else fieldErrorStart + errMsg + fieldEnd
    else if (styles.contains(ShowErrors) && !containsError(styles))
       reqHelper(rules, fieldErrorStart, fieldEnd)
    else ""
  }

  // Helps build the html for a field
  private def styleHelper(html: String, styles: List[Style], rules: List[Rule]): String = {
    def filterStyles(sl: List[Style]): List[Style] = {
      if (sl.contains(ShowErrors)) sl.filter(s => s != ShowRequirements).distinct
      else sl.distinct
    }

    def innerStyleHelper(i_html: String, i_styles: List[Style], i_rules: List[Rule]): String = i_styles match {
      case Label(label, placement) :: xs => placement match {
        case Before => innerStyleHelper(label + i_html, xs, i_rules)
        case After => innerStyleHelper(i_html, xs, i_rules) + label
        // HTML 5 feature
        case Inside => innerStyleHelper(i_html + " placeholder=\"" + label + "\"", xs, i_rules)
      }
      case SameLine :: xs => innerStyleHelper(i_html, xs, i_rules)

      // Radio button checked
      case Checked :: xs => innerStyleHelper(i_html + " checked", xs, i_rules)
      
      // Error related cases
      case Error(msg) :: xs => innerStyleHelper(i_html, xs, i_rules) + ruleHelper(msg, filterStyles(styles), rules)
      case ShowRequirements :: xs => innerStyleHelper(i_html, xs, i_rules) + ruleHelper("", filterStyles(styles), rules)
      case ShowErrors :: xs => innerStyleHelper(i_html, xs, i_rules) + ruleHelper("", filterStyles(styles), rules)
      case _ => i_html + ">"
    }
    innerStyleHelper(html, filterStyles(styles), rules)
  } 
}