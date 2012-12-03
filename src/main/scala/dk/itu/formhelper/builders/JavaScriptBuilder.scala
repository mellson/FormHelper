package dk.itu.formhelper.builders

object JavaScriptBuilder {
  private def scriptTag(s: String) = {
    "\n" +
    "<script>" +
    s +
    "</script>"
  }
  
  private def emailValidation: String = """
    function validateemail(name) {
        var x=document.getElementById(name);
        x.value=x.value.toUpperCase();
		return x.value==10;
    }
    """ 
  
  def testValidation: String = {
    scriptTag(emailValidation)
  } 
}