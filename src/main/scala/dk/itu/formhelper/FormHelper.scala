package dk.itu.formhelper

import dk.itu.formhelper.builders._ 

object FormHelper extends Styles with Rules {
  // TODO Refactor this into a HtmlHelper Trait
  final case class FormHtml(form: Form) {
    def plain = HtmlBuilder.plainHtml(form)
    def withValidation = HtmlBuilder.htmlWithValidation(form)
    def validationScript = "validation"
  }
  
  final case class Form(name: String, method: Method, action: String, fields: Field*) {
    def html = FormHtml(this)
    
    def validatedForm(postData: Option[Map[String, Seq[String]]]) : (Boolean, Form)= {
      val postForm = ScalaBuilder.formFromPost(this, postData)
      val boolean = ScalaBuilder.validateForm(postForm)
      (boolean,postForm)
    }
  }
  
  trait Method
  case object Get extends Method
  case object Post extends Method
  
  trait Field {
    val fname: String
    val value: String
    val styles: List[Style]
    val rules: List[Rule]
    
    def addRule(r: Rule): Field
    def addStyle(s: Style): Field
    def setValue(v: String): Field
    
    def id: String
    
    def html: String = HtmlBuilder.htmlField(this, false)
  }
  
  case class Submit(fname: String = "", value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field {
    def addRule(r: Rule) = this
    def addStyle(s: Style) = this
    def setValue(v: String) = this
    def id = fname
  }
  
  case class Text(fname: String, value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field {
    def addRule(r: Rule): Text = Text(fname, value, styles, r ::  rules)
    def addStyle(s: Style): Text = Text(fname, value, s ::  styles, rules)
    def setValue(v: String): Text = Text(fname, v, styles, rules)
    def matches(that: Text) = Text(fname, value, styles, Matches(that) :: rules)
    def id = fname
  }

  case class Password(fname: String, value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field {
    def addRule(r: Rule): Password = Password(fname, value, styles, r :: rules)
    def addStyle(s: Style): Password = Password(fname, value, s :: styles, rules)
    def setValue(v: String): Password = Password(fname, v, styles, rules)
    def matches(that: Password) = Password(fname, value, styles, Matches(that) :: rules)
    def id = fname
  }

  // fname is the group name for a Radio button group
  case class Radio(fname: String, value: String, styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field {
    def addRule(r: Rule): Radio = Radio(fname, value, styles, r :: rules)
    def addStyle(s: Style): Radio = Radio(fname, value, s :: styles, rules)
    def setValue(v: String): Radio = Radio(fname, value, Checked :: styles, rules)
    def matches(that: Radio) = Radio(fname, value, styles, Matches(that) :: rules)
    def id = fname+value
  }
}

