package dk.itu.formhelper

import dk.itu.formhelper.FormHelper._

object TestRunner extends App {
  def testForm = Form(
    name = "UserForm",
    method = Post,
    action = "/submit",
    Radio("musik","ja") withRule Required,
    Radio("musik","nej"),
//    Text("username") withRule (Length > 3 withError "Your username needs to be longer") && (Length < 10 withError "Your username is now too long"),
//    Text("age") withRule (Value > 5 withError "You have to be older than that to play with this form") && (Value < 99 withError "Don’t waste your last time on computers"),
//    Text("internet") withStyle Label <> "Do You Have Internet Access?",
//    Text("email") withRule ShowWhen("internet", StringValue === "yes") && (Regex === Email withError "Please provide a valid email address"),
    Submit("Submit Info")
  )

  println(testForm.htmlWithValidation)
}