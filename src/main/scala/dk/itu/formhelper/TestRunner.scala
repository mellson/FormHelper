package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Password("kode"),
    Text("test") withStyle Label <> "Ja" withRule DoubleValue === 40.5,
    Submit("Send Information")
  )

  //  println(testForm.htmlWithValidation)

}