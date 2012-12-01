package dk.itu.formhelper.builders
import dk.itu.formhelper.FormHelper._

object ScalaBuilder {
  // Validate rules and matches on a field
  // TODO Fix errors here, field value validates the rule even though it is wrong
//  def validateField(field: Field, form: Form): Boolean = {
//    val ruleBooleans = for {
//      rule <- field.rules
//    } yield fieldRuleValidator(field, rule, form)
//
//    for (r <- field.rules)
//      println("validated : " + r.validate(field.value))
//      
//    val matchBoolean = fieldMatchValidator(field, form)
//    !ruleBooleans.contains(false) && matchBoolean
//  }
//
//  // Validate rule and matches on a form
//  def validateForm(form: Form): Boolean = {
//    val matchBooleans = for {
//      field <- form.fields
//    } yield fieldMatchValidator(field, form)
//
//    val booleans = for {
//      field <- form.fields
//      rule <- field.rules
//    } yield fieldRuleValidator(field, rule, form)
//
//    !booleans.contains(false) && !matchBooleans.contains(false)
//  }
//
//  def formFromPost(form: Form, postData: Option[Map[String, Seq[String]]]): Form = {
//    // Checks if the data value should be added. Need this check because Radio fields are a bit weird.
//    def dataHelper(field: Field, data: String, fname: String) = field match {
//      case Radio(groupname, value, _, _, _) =>
//        if (groupname == fname && data == value) field setValue data else field
//      case _ => field setValue data
//    }
//
//    // Get all the fields that has data in the post
//    val dataFields = for {
//      dataMap <- postData.toList
//      (fname, dataSeq) <- dataMap
//      fields = form.fields.filter(field => field.fname == fname)
//      field <- fields
//      data <- dataSeq
//      fieldWithData = dataHelper(field, data, fname)
//    } yield fieldWithData
//
//    // If there was fields with data combine them with the rest of the fields
//    if (!dataFields.isEmpty) {
//      val newFields = for {
//        field <- form.fields
//        newfields = if (!dataFields.filter(f => f.id == field.id).isEmpty) dataFields.filter(f => f.id == field.id) else List(field)
//        newfield <- newfields
//        f = if (validateField(field, form)) newfield else newfield addStyle ShowErrors
//      } yield f
//
//      Form(form.name, form.method, form.action, newFields: _*)
//    } else form
//  }
//  
//    // Helper method for fieldMatchValidator
//  private def matchValidatorHelper(m: Match, value1: String, value2: String): Boolean = m match {
//    case Matcher(_, comparison, matchType) => matchType match {
//      case Length => comparison match {
//        case Below => value1.length < value2.length
//        case BelowOrEqual => value1.length <= value2.length
//        case Above => value1.length > value2.length
//        case AboveOrEqual => value1.length >= value2.length
//        case Equal => value1.length == value2.length
//      }
//      case Value => comparison match {
//        case Below => value1 < value2
//        case BelowOrEqual => value1 <= value2
//        case Above => value1 > value2
//        case AboveOrEqual => value1 >= value2
//        case Equal => value1 == value2
//      }
//    }
//  }
//  
//  // Validate all rules on a field
//  private def fieldRuleValidator(field: Field, rule: Rule, form: Form): Boolean = (field, rule) match {
//    case (Radio(_, _, _, _, _), Required) => !form.fields.filter(f => f.fname == field.fname).filter(f => f.styles.contains(Checked)).isEmpty
//    case _ => rule.validate(field.value)
//  }
//
//  // Validate all matches on a field
//  private def fieldMatchValidator(field: Field, form: Form): Boolean = {
//    val matches = for {
//      matchId <- field.matches
//      fieldMatch <- form.fields.filter(f => f.id == matchId.fieldId)
//    } yield matchValidatorHelper(matchId, field.value, fieldMatch.value)
//    !matches.contains(false)
//  }
}