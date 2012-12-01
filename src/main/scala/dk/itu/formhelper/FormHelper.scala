package dk.itu.formhelper

import dk.itu.formhelper.builders._ 

object FormHelper extends Styles with Rules with Matches {
  final case class Form(name: String, method: Method, action: String, fields: Field*) {
    def html = HtmlBuilder.plainHtml(this)
    def htmlWithValidation = HtmlBuilder.htmlWithValidation(this)
    def jsValidationsScript = "validation"
    
    def validatedForm(postData: Option[Map[String, Seq[String]]]) : (Boolean, Form)= {
      val postForm = ScalaBuilder.formFromPost(this, postData)
      val boolean = ScalaBuilder.validateForm(postForm)
      (boolean,postForm)
    }
  }
  
  sealed abstract class Method
  case object Get extends Method
  case object Post extends Method
  
  trait Field {
    val fname: String
    def value: String
    val styles: List[Style]
    val rules: List[Rule]
    val matches: List[Match]
    
    def addRule(r: Rule): Field
    def addStyle(s: Style): Field
    def addMatch(m: Match): Field
    def setValue(v: String): Field
    
    def id: String
    def inputType: String
    
    def html: String = HtmlBuilder.htmlField(this, false)
    def htmlWithValidation: String = HtmlBuilder.htmlField(this, true)
    def jsValidationsScript: String = "validation"
  }
  
  case class Submit(fname: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil, matches: List[Match] = Nil) extends Field {
    def addRule(r: Rule) = this
    def addStyle(s: Style) = this
    def addMatch(m: Match) = this
    def setValue(v: String) = this
    def id = fname
    def inputType = "submit"
    def value = fname
  }
  
  case class Text(fname: String, value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil, matches: List[Match] = Nil) extends Field {
    def addRule(r: Rule): Text = Text(fname, value, styles, r ::  rules, matches)
    def addStyle(s: Style): Text = Text(fname, value, s ::  styles, rules, matches)
    def addMatch(m: Match): Text = Text(fname, value, styles, rules, m :: matches)
    def setValue(v: String): Text = Text(fname, v, styles, rules, matches)
    def id = fname
    def inputType = "text"
  }

  case class Password(fname: String, value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil, matches: List[Match] = Nil) extends Field {
    def addRule(r: Rule): Password = Password(fname, value, styles, r :: rules, matches)
    def addStyle(s: Style): Password = Password(fname, value, s :: styles, rules, matches)
    def addMatch(m: Match): Password = Password(fname, value, styles, rules, m :: matches)
    def setValue(v: String): Password = Password(fname, v, styles, rules, matches)
    def id = fname
    def inputType = "password"
  }

  // fname is the group name for a Radio button group
  case class Radio(fname: String, value: String, styles: List[Style] = Nil, rules: List[Rule] = Nil, matches: List[Match] = Nil) extends Field {
    def addRule(r: Rule): Radio = Radio(fname, value, styles, r :: rules, matches)
    def addStyle(s: Style): Radio = Radio(fname, value, s :: styles, rules, matches)
    def addMatch(m: Match): Radio = Radio(fname, value, styles, rules, m :: matches)
    def setValue(v: String): Radio = Radio(fname, value, Checked :: styles, rules, matches)
    def id = fname+value
    def inputType = "radio"
  }
}

