package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Text("user"),
    Text("field1") withRule ShowWhen("user", Value === "Scala") && Length > 2,
    Submit("Send Information")
  )

//  Form(
//    name = "Example Form",
//    method = Post,
//    action = "/submit",
//    Text("username") withRule Length > 2 && Length < 20,
//    Password("password") withRule  Required &&Error "You must supply a password",
//    Submit("Submit Login")
//  )

  println(testForm.htmlWithValidation)
}