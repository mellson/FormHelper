package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Text("user") withRule Length > 10,
    Text("field1") withRule ShowWhen(FieldId("user"), Value === "hej", "Custom err here") && Length > 2,
    Submit("Send Information")
  )

  println(testForm.htmlWithValidation)
}