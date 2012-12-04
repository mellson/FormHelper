package dk.itu.formhelper

import builders.JavaScriptBuilder
import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Form",
    method = Post,
    action = "/test",
    Text("Email") withStyle Label <> "Anders",
    Text("Email2") withStyle Label <> "Anders",
    Submit("Send information")
  )

  println(JavaScriptBuilder.validationScriptForForm(testForm))
}