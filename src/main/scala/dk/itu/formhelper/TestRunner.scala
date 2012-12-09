package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Checkbox("gruppe1", "ja") withStyle Label > "Ja1",
    Checkbox("gruppe1", "nej") withStyle Label > "Nej1" withRule Required,
    Submit("Send Information")
  )

  //  println(testForm.htmlWithValidation)

}