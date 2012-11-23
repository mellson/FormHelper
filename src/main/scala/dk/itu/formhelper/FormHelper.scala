package dk.itu.formhelper

import dk.itu.formhelper.builders._ 

object FormHelper extends Fields {
  final case class Form[U](name: String, method: Method, action: String, fields: Field[U]*) {
    def html: String = HtmlBuilder.htmlForm(this)
    def scala = ScalaBuilder.validator(this)
  }
  
  trait Method
  case object Get extends Method
  case object Post extends Method
}