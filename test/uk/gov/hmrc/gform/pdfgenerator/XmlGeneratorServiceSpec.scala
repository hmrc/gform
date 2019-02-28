/*
 * Copyright 2019 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.{ SectionFormField, SubmissionRef }
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.immutable.List
import scala.xml.{ Elem, Utility }

class XmlGeneratorServiceSpec extends Spec {

  //TODO: benefit from ExampleData

  "SubmissionServiceHelper.getSectionFormFields" should "find repeating group fields" in {

    val formFields = Seq[FormField](
      FormField(FormComponentId("UNO"), "UNO"),
      FormField(FormComponentId("1_UNO"), "1_UNO"),
      FormField(FormComponentId("2_UNO"), "2_UNO"),
      FormField(FormComponentId("3_UNO"), "3_UNO"),
      FormField(FormComponentId("4_UNO"), "4_UNO"),
      FormField(FormComponentId("DOS"), "DOS"),
      FormField(FormComponentId("1_DOS"), "1_DOS"),
      FormField(FormComponentId("2_DOS"), "2_DOS"),
      FormField(FormComponentId("3_DOS"), "3_DOS"),
      FormField(FormComponentId("4_DOS"), "4_DOS")
    )
    val formData = FormData(formFields)

    val sectionFormFields = List(
      SectionFormField(
        "Section title",
        List(
          (
            List(FormField(FormComponentId("UNO"), "UNO")),
            FormComponent(
              FormComponentId("UNO"),
              Text(AnyText, Value),
              "Submissible text label",
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            )),
          (
            List(FormField(FormComponentId("DOS"), "DOS")),
            FormComponent(
              FormComponentId("DOS"),
              Text(AnyText, Value),
              "Submissible text label",
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            )),
          (
            List(FormField(FormComponentId("1_UNO"), "1_UNO")),
            FormComponent(
              FormComponentId("1_UNO"),
              Text(AnyText, Value),
              "Non-submissible text label",
              None,
              None,
              None,
              true,
              true,
              submissible = false,
              derived = false,
              onlyShowOnSummary = false,
              None
            )),
          (
            List(FormField(FormComponentId("1_DOS"), "1_DOS")),
            FormComponent(
              FormComponentId("1_DOS"),
              Text(AnyText, Value),
              "Submissible text label",
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            ))
        )
      ),
      SectionFormField(
        "Declaration",
        List(
          (
            List(FormField(FormComponentId("TRES"), "TRES")),
            FormComponent(
              FormComponentId("TRES"),
              Text(AnyText, Constant("TRES")),
              "Submissible text label",
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            )))
      )
    )

    val expected =
      <documents>
        <document>
          <header>
            <title>CFJ-7CEH-P7R</title>
            <source>gform</source>
            <target>DMS</target>
          </header>
          <submission>
            <attribute>
              <attribute_name>UNO</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>UNO</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>DOS</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>DOS</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>1_DOS</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>1_DOS</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>TRES</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>TRES</attribute_value>
              </attribute_values>
            </attribute>
          </submission>
        </document>
      </documents>

    val submissionRef = SubmissionRef("CFJ-7CEH-P7R")

    val dataXml = XmlGeneratorService.getXml(sectionFormFields, submissionRef)

    dataXml should equal(Utility.trim(expected).asInstanceOf[Elem])(after being streamlined[Elem])
  }

  implicit lazy val hc = new HeaderCarrier()
}
