package dk.itu.formhelper.builders
import dk.itu.formhelper.FormHelper._
import dk.itu.formhelper.Validator._

object ScalaBuilder {
  def validateForm(form: Form): Boolean = {
    val errOptions = (for {
      field <- form.fields
    } yield fieldEval(field.rule, field, form)).toList
    
    def errorsIn(errs: List[Option[String]]): Boolean = errs match {
      case Nil => false
      case None::xs => errorsIn(xs)
      case Some(err)::xs => true
    }

    !errorsIn(errOptions)
  }

  def formFromPost(form: Form, postData: Option[Map[String, Seq[String]]]): Form = {
    // Checks if the data value should be added. Need this check because Radio fields are a bit weird.
    def dataHelper(field: Field, data: String, name: String): Field = field match {
      case Radio(groupname, value, _, _) =>
        // Add checked to the radio that is checked
        if (groupname == name && data == value) field setValue data
        else field
      case _ => field setValue data
    }

    /* If a field is a member of a group that has a required rule, add that rule to all members of the group
     * The method takes a field and returns a field with the added rule if necessary
     */
    def addGroupRequired(field: Field): Field = field match {
      case Radio(_, _, _, _) =>
        val groupContainsRequired = !form.fields.filter(f => f.name == field.name).filter(f => ruleList(f.rule).contains(Required)).isEmpty
        val selfContainsRequired = ruleList(field.rule).contains(Required)
        if (groupContainsRequired)
          if (selfContainsRequired) field
          else field withRule Required
        else field
      case _ => field
    }

    // Get all the fields that has data in the post
    val dataFields = for {
      dataMap <- postData.toList
      (name, dataSeq) <- dataMap
      fields = form.fields.filter(field => field.name == name)
      field <- fields
      data <- dataSeq
      fieldWithData = dataHelper(field, data, name)
    } yield fieldWithData

    // If there was fields with data combine them with the rest of the fields
    if (!dataFields.isEmpty) {
      val newFields = for {
        field <- form.fields
        newfields = if (!dataFields.filter(f => f.id == field.id).isEmpty) dataFields.filter(f => f.id == field.id) else List(field)
        newfield <- newfields
      } yield newfield
      
      // Temp form with the data from the post
      val tempForm = Form(form.name, form.method, form.action, newFields: _*)

      // Check if the new form with data fullfills all requirements, if not add error style
      val errorFields = for {
        field <- form.fields.map(f => addGroupRequired(f))
        newfields = if (!dataFields.filter(f => f.id == field.id).isEmpty) dataFields.filter(f => f.id == field.id) else List(field)
        newfield <- newfields
        f = if (fieldEval(newfield.rule, newfield, tempForm) == None) newfield else newfield withStyle Error === fieldEval(newfield.rule, newfield, tempForm).get
      } yield f

      Form(form.name, form.method, form.action, errorFields: _*)
    } else form
  }
}