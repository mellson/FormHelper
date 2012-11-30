package dk.itu.formhelper.builders
import dk.itu.formhelper.FormHelper._

object ScalaBuilder {
  private def fieldRuleValidator(field: Field, rule: Rule, form: Form): Boolean = (field, rule) match {
    case (Radio(_, _, _, _, _), Required) => !form.fields.filter(f => f.fname == field.fname).filter(f => f.styles.contains(Checked)).isEmpty
    case _ => rule.validate(field.value)
  }

  private def fieldMatchValidator(field: Field, form: Form): Boolean = {
    val matches = for {
      matchId <- field.matches
      fieldMatch <- form.fields.filter(f => f.id == matchId.fieldId)
    } yield fieldMatch.value == field.value
    !matches.contains(false)
  }

  def validateField(field: Field, form: Form): Boolean = {
    val ruleBooleans = for {
      field <- form.fields
      rule <- field.rules
    } yield fieldRuleValidator(field, rule, form)

    val matchBoolean = fieldMatchValidator(field, form)

    !ruleBooleans.contains(false) && matchBoolean
  }

  def validateForm(form: Form): Boolean = {
    val matchBooleans = for {
      field <- form.fields
    } yield fieldMatchValidator(field, form)

    val booleans = for {
      field <- form.fields
      rule <- field.rules
    } yield fieldRuleValidator(field, rule, form)

    !booleans.contains(false) && !matchBooleans.contains(false)
  }

  def formFromPost(form: Form, postData: Option[Map[String, Seq[String]]]): Form = {
    // Checks if the data value should be added. Need this check because Radio fields are a bit weird.
    def dataHelper(field: Field, data: String, fname: String) = field match {
      case Radio(groupname, value, _, _, _) =>
        if (groupname == fname && data == value) field setValue data else field
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

    // If there was fields with data combine them with the rest of the fields
    if (!dataFields.isEmpty) {
      val newFields = for {
        field <- form.fields
        newfields = if (!dataFields.filter(f => f.id == field.id).isEmpty) dataFields.filter(f => f.id == field.id) else List(field)
        newfield <- newfields
        f = if (validateField(field, form)) newfield else newfield addStyle ShowErrors
      } yield f

      Form(form.name, form.method, form.action, newFields: _*)
    } else form
  }
}