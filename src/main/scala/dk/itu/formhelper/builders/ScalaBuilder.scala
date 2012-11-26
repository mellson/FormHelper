package dk.itu.formhelper.builders
import dk.itu.formhelper.FormHelper._

object ScalaBuilder {
  def validateForm(form: Form): Boolean = {
    def fieldValidator(field: Field, rule: Rule) = (field,rule) match {
      case (Radio(_,_,_,_),Required) => !form.fields.filter(f=>f.fname==field.fname).filter(f=>f.styles.contains(Checked)).isEmpty
      case _ => rule.validate(field.value)
    }
    
    val booleans = for {
      field <- form.fields
      rule <- field.rules
    } yield fieldValidator(field, rule)
    !booleans.contains(false)
  }
  
  def formFromPost(form: Form, postData: Option[Map[String, Seq[String]]]): Form = { 

    // Checks if the data value should be added. Need this check because Radio fields are a bit weird.
    def dataHelper(field: Field, data: String, fname: String) = field match {
      case Radio(groupname,value,_,_) =>
        if (groupname==fname && data==value) field setValue data else field
      case _ => field setValue data
    }
    
    // Get all the fields that has data in the post
    val dataFields = for {
      dataMap <- postData.toList
      (fname, dataSeq) <- dataMap
      fields = form.fields.filter(field => field.fname == fname)
      field <- fields
      data <- dataSeq
      fieldWithData = dataHelper(field, data, fname)
    } yield fieldWithData
    
    
    
//    val errorFields = for {
//      field <- dataFields
//      rule <- field.rules
//      newfield = if (!rule.validate(field.value)) field addStyle ShowErrors else field
//    } yield newfield
//    
//    println(errorFields)
    
    // If there was fields with data combine them with the rest of the fields
    if (!dataFields.isEmpty) {
      val newFields = for {
        field <- form.fields
        newfields = if (!dataFields.filter(f => f.id == field.id).isEmpty) dataFields.filter(f => f.id == field.id) else List(field)
        newfield <- newfields
        f = if (newfield.rules.filter(rule=>(!rule.validate(newfield.value))).isEmpty) newfield else newfield addStyle ShowErrors
      } yield f

      Form(form.name, form.method, form.action, newFields: _*)
    } else form
  }
}