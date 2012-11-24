package dk.itu.formhelper

trait Fields extends Rules with Styles {
  trait Field[+T] {
    val fname: String
    val value: String
    val styles: List[Style]
    val rules: List[Rule]
    
    def addRule(r: Rule): Field[T]
    def addStyle(s: Style): Field[T]
    def setValue(v: String): Field[T]
    
    def id: String
  }
  
  case class Submit(fname: String = "", value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Submit] {
    def addRule(r: Rule) = this
    def addStyle(s: Style) = this
    def setValue(v: String) = this
    def id = fname
  }
  
  case class Text(fname: String, value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Text] {
    def addRule(r: Rule): Text = Text(fname, value, styles, r ::  rules)
    def addStyle(s: Style): Text = Text(fname, value, s ::  styles, rules)
    def setValue(v: String): Text = Text(fname, v, styles, rules)
    def matches(that: Text) = Text(fname, value, styles, Matches(that) :: rules)
    def id = fname
  }

  case class Password(fname: String, value: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Password] {
    def addRule(r: Rule): Password = Password(fname, value, styles, r :: rules)
    def addStyle(s: Style): Password = Password(fname, value, s :: styles, rules)
    def setValue(v: String): Password = Password(fname, v, styles, rules)
    def matches(that: Password) = Password(fname, value, styles, Matches(that) :: rules)
    def id = fname
  }

  // fname is the group name for a Radio button group
  case class Radio(fname: String, value: String, styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Radio] {
    def addRule(r: Rule): Radio = Radio(fname, value, styles, r :: rules)
    def addStyle(s: Style): Radio = Radio(fname, value, s :: styles, rules)
    def setValue(v: String): Radio = Radio(fname, value, Checked :: styles, rules)
    def matches(that: Radio) = Radio(fname, value, styles, Matches(that) :: rules)
    def id = fname+value
  }
}