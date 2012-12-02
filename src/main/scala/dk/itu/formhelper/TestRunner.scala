package dk.itu.formhelper
import dk.itu.formhelper.FormHelper._ 

object TestRunner extends App {
  val form = Form(
    name = "Else Form",
    method = Post,
    action = "routes.Application.submit().toString",
    Text("username") withStyle Label <> "Brugernavn" withRule Length > 2 withRule Length < 5,
    Text("hurra") withStyle Label <> "Tester" withRule Length > FieldId("username"),
    Submit("Send information")
    )
  
      
  println(formHtml(form, false))
}