package dk.itu.formhelper

import builders.JavaScriptBuilder
import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Dittes",
    method = Post,
    action = "/test",
    Text("Email") withStyle Label <> "Email" withRule OK,
    Submit("Send information")
  )

  println(testForm.htmlWithValidation)
//  println(JavaScriptBuilder.validationScriptForForm(testForm))
}