/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.services

import play.twirl.api.Html
import uk.gov.hmrc.gform.models._
import scala.collection.immutable.List
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object HtmlGeneratorService extends HtmlGeneratorService {}

trait HtmlGeneratorService {
  def generateDocumentHTML(sectionFormFields: List[SectionFormField], formName: String): String = {
    val sectionsHtml = sectionFormFields.map(generateSectionHtml(_))
    uk.gov.hmrc.gform.views.html.pdfGeneration.document(getEnglishText(formName), sectionsHtml).body
  }

  private def generateSectionHtml(section: SectionFormField): Html = {
    val elementsHtml = section.fields.filter { case (_, fieldValue) => fieldValue.submissible }.map {
      generateElementHtml(_)
    }
    uk.gov.hmrc.gform.views.html.pdfGeneration.section(getEnglishText(section.title), elementsHtml)
  }

  private def generateElementHtml(arg: (List[FormField], FieldValue)) = arg match {
    case (formFields, fieldValue) =>
      val (name, value) = fieldValue.`type` match {
        case choice @ Choice(_, _, _, _, _) => generateChoiceFieldHTML(choice, fieldValue, formFields.head)
        case Date(_, _, _) => generateDateFieldHTML(fieldValue, formFields)
        case Address(_) => generateAddressFieldHTML(fieldValue, formFields)
        case _ => (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), Html(getEnglishText(formFields.head.value)))
      }
      uk.gov.hmrc.gform.views.html.pdfGeneration.element(name, value)
  }

  private def generateChoiceFieldHTML(choiceElement: Choice, fieldValue: FieldValue, formField: FormField) = {
    val selections = formField.value.split(",").toList
    val optionsAsMap = choiceElement.options.zipWithIndex.map {
      case (option, index) => index.toString -> getEnglishText(option)
    }.toList.toMap
    val values = Html(selections.flatMap(selection => optionsAsMap.get(selection)).mkString("<BR>"))
    (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), values)
  }

  private def generateDateFieldHTML(fieldValue: FieldValue, formFields: List[FormField]) = {
    val day = formFields.filter(_.id.value.endsWith("day")).head.value
    val month = formFields.filter(_.id.value.endsWith("month")).head.value
    val year = formFields.filter(_.id.value.endsWith("year")).head.value

    val date = DateTime.parse(s"${year}-${month}-${day}")
    val formatter = DateTimeFormat.forPattern("dd MMMM yyyy")
    (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), Html(formatter.print(date)))
  }

  private def generateAddressFieldHTML(fieldValue: FieldValue, formFields: List[FormField]) = {
    val value = Address.fields(fieldValue.id).filterNot(_.value.endsWith("uk")).map { addressFieldId =>
      formFields.filter(_.id.equals(addressFieldId)).head.value
    }.mkString("<BR>")

    (getEnglishText(fieldValue.shortName.getOrElse(fieldValue.label)), Html(value))
  }

  private def getEnglishText(pipeSeparatedTranslations: String) = {
    pipeSeparatedTranslations.split(raw"\|").head
  }
}
