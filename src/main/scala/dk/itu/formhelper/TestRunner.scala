package dk.itu.formhelper
import FormHelper._

object TestRunner extends App {
  val exform = Form(
    name = "super form",
    method = Post,
    action = "superform.php",
    Text("username") addStyle Label <> "Name", 
    Text("age") addStyle Label <> "Age",
    Submit("Send info")
    )

  println(exform.html)
}