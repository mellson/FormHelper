package dk.itu.formhelper
import FormHelper._

object TestRunner extends App {
  val exform = Form(
    name = "super form",
    method = Post,
    action = "/submit",
    Text("username") addStyle Label <> "Name",
    Text("age") addStyle Label <> "Age",
    Submit("Send info")
    )
    
  val field = Text("username")
//  println(field.html)

  exform.html.withValidation
  println(exform.html.plain)
}