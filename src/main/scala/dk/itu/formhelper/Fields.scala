package dk.itu.formhelper

trait Fields extends Rules with Styles {
  trait Field[+T] {
    val styles: List[Style]
    val rules: List[Rule]
    
    def addRule(r: Rule): T
    def addStyle(s: Style): T
  }
  
  case class Submit(fname: String = "", styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Submit] {
    def addRule(r: Rule) = this
    def addStyle(s: Style) = this
  }
  
  case class Text(fname: String, styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Text] {
    def addRule(r: Rule): Text = Text(fname, styles, r ::  rules)
    def addStyle(s: Style): Text = Text(fname, s ::  styles, rules)
    def matches(that: Text) = Text(fname, styles, Matches(that) :: rules)
  }

  case class Password(fname: String, styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Password] {
    def addRule(r: Rule): Password = Password(fname, styles, r :: rules)
    def addStyle(s: Style): Password = Password(fname, s :: styles, rules)
    def matches(that: Password) = Password(fname, styles, Matches(that) :: rules)
  }

  case class Radio(group: String, value: String, styles: List[Style] = Nil, rules: List[Rule] = Nil) extends Field[Radio] {
    def addRule(r: Rule): Radio = Radio(group, value, styles, r :: rules)
    def addStyle(s: Style): Radio = Radio(group, value, s :: styles, rules)
    def matches(that: Radio) = Radio(group, value, styles, Matches(that) :: rules)
  }
}