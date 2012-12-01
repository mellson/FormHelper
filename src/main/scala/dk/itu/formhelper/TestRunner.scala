package dk.itu.formhelper
import dk.itu.formhelper.FormHelper._ 

object TestRunner extends App {
  val form = Form(
      name = "Form",
      method = Post,
      action = "/",
      Text("username") withStyle Label <> "Hurra",
      Password("password") withRule Length < 6, 
      Radio("hurra", "super") setChecked(true) withStyle Label < "Super"
      )
  
//  val field = Text("username") withRule Length === 5 withStyle Label > "hej" && ShowRequirements withRule Length < 40
      
  println(formHtml(form, false))
}