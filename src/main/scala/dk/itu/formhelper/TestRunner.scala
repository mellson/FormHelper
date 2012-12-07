package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "Super Cool Form",
    method = Post,
    action = "/test",
    Radio("gruppe1","ja") withRule Required,
    Radio("gruppe1","nej"),
    Text("hej") withRule Required,
    Text("Anders") withRule IntValue === 40,
    Submit("Send Information")
  )

  println(testForm.htmlWithValidation)

}