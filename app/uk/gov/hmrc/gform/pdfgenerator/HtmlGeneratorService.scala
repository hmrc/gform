/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.pdfgenerator

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.SectionFormField

import scala.collection.immutable.List

object HtmlGeneratorService extends HtmlGeneratorService {}

trait HtmlGeneratorService {

  def generateDocumentHTML(sectionFormFields: List[SectionFormField], formName: String, formData: FormData): String = {
    val html = sectionFormFields.map(generateSectionHtml)
    uk.gov.hmrc.gform.views.html.pdfGeneration.document(getEnglishText(formName), html).body
  }

  private def generateSectionHtml(section: SectionFormField): Html = {
    val elementsHtml = section.fields.filter { case (_, fieldValue) => fieldValue.submissible }.map(generateElementHtml)
    uk.gov.hmrc.gform.views.html.pdfGeneration.section(getEnglishText(section.title), elementsHtml)
  }

  private def generateElementHtml(arg: (List[FormField], FormComponent)) = arg match {
    case (formFields, fieldValue) =>
      val (name, value) = fieldValue.`type` match {
        case choice @ Choice(_, _, _, _, _) => generateChoiceFieldHTML(choice, fieldValue, formFields.head)
        case UkSortCode(_)                  => generateSortCodeFieldHTML(fieldValue, formFields)
        case Date(_, _, _)                  => generateDateFieldHTML(fieldValue, formFields)
        case Address(_)                     => generateAddressFieldHTML(fieldValue, formFields)
        case text @ Text(_, _)              => generateTextFieldHTML(text, fieldValue, formFields.head.value)
        case _                              => (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), Html(formFields.head.value))
      }
      uk.gov.hmrc.gform.views.html.pdfGeneration.element(name, value)
  }

  private def generateTextFieldHTML(textElement: Text, fieldValue: FormComponent, formFieldValue: String) = {
    val name = getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label))
    val value = textElement.constraint match {
      case Number(_, _, _) | PositiveNumber(_, _, _) => TextConstraint.filterNumberValue(formFieldValue)
      case _                                         => formFieldValue
    }
    (name, Html(value))
  }

  private def generateChoiceFieldHTML(choiceElement: Choice, fieldValue: FormComponent, formField: FormField) = {
    val selections = formField.value.split(",").toList
    val optionsAsMap = choiceElement.options.zipWithIndex
      .map {
        case (option, index) => index.toString -> getEnglishText(option)
      }
      .toList
      .toMap
    val values = Html(selections.flatMap(selection => optionsAsMap.get(selection)).mkString("<BR>"))
    (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), values)
  }

  private def generateSortCodeFieldHTML(fieldValue: FormComponent, formFields: List[FormField]) = {
    val sortCodeValue = UkSortCode.fields(fieldValue.id).map(x => formFields.filter(_.id == x).head.value).mkString("-")
    val values = Html(sortCodeValue)
    (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), values)
  }

  private def generateDateFieldHTML(fieldValue: FormComponent, formFields: List[FormField]) = {
    val day = formFields.filter(_.id.value.endsWith("day")).head.value
    val month = formFields.filter(_.id.value.endsWith("month")).head.value
    val year = formFields.filter(_.id.value.endsWith("year")).head.value

    val date = DateTime.parse(s"$year-$month-$day")
    val formatter = DateTimeFormat.forPattern("dd MMMM yyyy")
    (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), Html(formatter.print(date)))
  }

  private def generateAddressFieldHTML(fieldValue: FormComponent, formFields: List[FormField]) = {
    val value = Address
      .fields(fieldValue.id)
      .filterNot(_.value.endsWith("uk"))
      .map { addressFieldId =>
        formFields.filter(_.id.equals(addressFieldId)).head.value
      }
      .mkString("<BR>")

    (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), Html(value))
  }

  private def getEnglishText(pipeSeparatedTranslations: String) =
    pipeSeparatedTranslations.split(raw"\|").head
}
