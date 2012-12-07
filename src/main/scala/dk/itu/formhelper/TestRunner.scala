package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Radio("gruppe1","ja") withRule Required,
    Radio("gruppe1","nej"),
    Text("hej"),
    Submit("Send information")
  )

  println(testForm.htmlWithValidation)
  //  println(JavaScriptBuilder.validationScriptForForm(testForm))
}