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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Spec
import org.jsoup.Jsoup
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AnyText
import uk.gov.hmrc.gform.pdfgenerator.HtmlGeneratorService
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.SectionFormField

import scala.collection.immutable.List

class HtmlGeneratorServiceSpec extends Spec {

  val testService = new HtmlGeneratorService {}
  override val formData = FormData(Seq.empty)

  "HtmlGeneratorService.generateDocumentHTML" should "return HTML containing the form title" in {
    val html = testService.generateDocumentHTML(Nil, "FORM TITLE", formData)
    val doc = Jsoup.parse(html)

    doc.getElementsByTag("title").html.equalsIgnoreCase("FORM TITLE") shouldBe true
    doc.getElementsByTag("h2").html.equalsIgnoreCase("FORM TITLE") shouldBe true
  }

  it should "return HTML containing section title" in {
    val formFields = List(SectionFormField("SECTION TITLE", Nil))
    val html = testService.generateDocumentHTML(formFields, "FORM TITLE", formData)
    val doc = Jsoup.parse(html)

    doc.getElementsByTag("th").html.equalsIgnoreCase("SECTION TITLE") shouldBe true
  }

  it should "return HTML containing formatted date" in {
    val fieldValue = FieldValue(
      id = FieldId("id"),
      `type` = Date(AnyDate, Offset(0), None),
      label = "label",
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val formList = List(
      FormField(FieldId("id.day"), "01"),
      FormField(FieldId("id.month"), "01"),
      FormField(FieldId("id.year"), "1970")
    )

    val formFields = List(SectionFormField("SECTION TITLE", List((formList, fieldValue))))
    val html = testService.generateDocumentHTML(formFields, "FORM TITLE", formData)
    val doc = Jsoup.parse(html)

    doc.getElementsByTag("td").last.html.equalsIgnoreCase("01 January 1970") shouldBe true
  }

  it should "return HTML containing address" in {
    val fieldValue = FieldValue(
      id = FieldId("id"),
      `type` = Address(false),
      label = "label",
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val formList = List(
      FormField(FieldId("id-street1"), "A1"),
      FormField(FieldId("id-street2"), "A2"),
      FormField(FieldId("id-street3"), "A3"),
      FormField(FieldId("id-street4"), "A4"),
      FormField(FieldId("id-postcode"), "PC"),
      FormField(FieldId("id-uk"), "true"),
      FormField(FieldId("id-country"), "")
    )

    val formFields = List(SectionFormField("SECTION TITLE", List((formList, fieldValue))))
    val html = testService.generateDocumentHTML(formFields, "FORM TITLE", formData)
    val doc = Jsoup.parse(html)

    doc.getElementsByTag("td").last.html.equalsIgnoreCase("A1<br>A2<br>A3<br>A4<br>PC<br>") shouldBe true
  }

  it should "return HTML containing choice selection" in {
    val fieldValue = FieldValue(
      id = FieldId("id"),
      `type` = Choice(Checkbox, NonEmptyList.of("One", "Two", "Three"), Vertical, Nil, None),
      label = "label",
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val formList = List(
      FormField(FieldId("id"), "0,2")
    )

    val formFields = List(SectionFormField("SECTION TITLE", List((formList, fieldValue))))
    val html = testService.generateDocumentHTML(formFields, "FORM TITLE", formData)
    val doc = Jsoup.parse(html)

    doc.getElementsByTag("td").last.html.equalsIgnoreCase("One<br>Three") shouldBe true
  }

  it should "return HTML containing text field" in {
    val fieldValue = FieldValue(
      id = FieldId("id"),
      `type` = Text(AnyText, Constant("CONSTANT")),
      label = "label",
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val formList = List(
      FormField(FieldId("id"), "Hello")
    )

    val formFields = List(SectionFormField("SECTION TITLE", List((formList, fieldValue))))
    val html = testService.generateDocumentHTML(formFields, "FORM TITLE", formData)
    val doc = Jsoup.parse(html)

    doc.getElementsByTag("td").last.html.equalsIgnoreCase("Hello") shouldBe true
  }
}
