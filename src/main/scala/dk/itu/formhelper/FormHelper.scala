package dk.itu.formhelper

import dk.itu.formhelper.builders._ 

object FormHelper extends Fields {
  final case class Form[T](name: String, method: Method, action: String, fields: Field[T]*) {
    def html: String = HtmlBuilder.htmlForm(this)
    
    def validatedForm(postData: Option[Map[String, Seq[String]]]) : (Boolean, Form[T])= {
      val postForm = ScalaBuilder.formFromPost(this, postData)
      val boolean = ScalaBuilder.validateForm(postForm)
      (boolean,postForm)
    }
  }
  
  trait Method
  case object Get extends Method
  case object Post extends Method
}