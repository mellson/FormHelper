package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Text("user"),
    Text("field1") withRule ShowWhen(FieldId("user"), Value === "Scala"),
    Submit("Send Information")
  )

  println(testForm.htmlWithValidation)
}