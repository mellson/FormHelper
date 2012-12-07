package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Text("Email") withStyle Label <> "Email" withRule Value === FieldId("Test"),
    Text("Test") withStyle Label <> "Test",
    Submit("Send information")
  )

  println(testForm.htmlWithValidation)
  //  println(JavaScriptBuilder.validationScriptForForm(testForm))
}