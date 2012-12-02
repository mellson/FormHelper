package dk.itu.formhelper
import dk.itu.formhelper.FormHelper._ 

object TestRunner extends App {
  val form = Form(
      name = "Form",
      method = Post,
      action = "/",
      Text("username") withRule Length === 4,
      Password("password") withRule Length < 6 
      )
  
      
  println(formHtml(form, false))
}