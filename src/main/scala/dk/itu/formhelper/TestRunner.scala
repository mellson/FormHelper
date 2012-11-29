package dk.itu.formhelper
import FormHelper._

object TestRunner extends App {
  val exform = Form(
    name = "super form",
    method = Post,
    action = "/submit",
    Text("username") addStyle Label <> "Name",
    Password("age") addStyle Label <> "Age",
    Radio("super","ja"),
    Submit("Send info")
    )
    
  val field = Text("username") addRule StringValue == "hej"
//  println(field.htmlWithValidation)
//
//  println(exform.htmlWithValidation)
  println(exform.fields.filter(f => f.id == "username"))
}