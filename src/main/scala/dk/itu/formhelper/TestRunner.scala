package dk.itu.formhelper
import dk.itu.formhelper.FormHelper._ 

object TestRunner extends App {
  val form = Form(
    name = "Else Form",
    method = Post,
    action = "routes.Application.submit().toString",
    Text("Email") withStyle Label <> "Email",
    Submit("Send information")
    )
      
  println(form.htmlWithValidation)
}