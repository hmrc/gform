/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.{ AtomicFormComponentFormFields, SectionFormFieldsByAtomicFormComponents }
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
      SectionFormFieldsByAtomicFormComponents(
        toSmartString("Section title"),
        List(
          AtomicFormComponentFormFields(
            FormComponent(
              FormComponentId("UNO"),
              Text(BasicText, Value),
              toSmartString("Submissible text label"),
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            ),
            NonEmptyList.of(FormField(FormComponentId("UNO"), "UNO"))
          ),
          AtomicFormComponentFormFields(
            FormComponent(
              FormComponentId("DOS"),
              Text(BasicText, Value),
              toSmartString("Submissible text label"),
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            ),
            NonEmptyList.of(FormField(FormComponentId("DOS"), "DOS"))
          ),
          AtomicFormComponentFormFields(
            FormComponent(
              FormComponentId("1_UNO"),
              Text(BasicText, Value),
              toSmartString("Non-submissible text label"),
              None,
              None,
              None,
              true,
              true,
              submissible = false,
              derived = false,
              onlyShowOnSummary = false,
              None
            ),
            NonEmptyList.of(FormField(FormComponentId("1_UNO"), "1_UNO"))
          ),
          AtomicFormComponentFormFields(
            FormComponent(
              FormComponentId("1_DOS"),
              Text(BasicText, Value),
              toSmartString("Submissible text label"),
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            ),
            NonEmptyList.of(FormField(FormComponentId("1_DOS"), "1_DOS"))
          )
        )
      ),
      SectionFormFieldsByAtomicFormComponents(
        toSmartString("Declaration"),
        List(
          AtomicFormComponentFormFields(
            FormComponent(
              FormComponentId("TRES"),
              Text(BasicText, Constant("TRES")),
              toSmartString("Submissible text label"),
              None,
              None,
              None,
              true,
              true,
              submissible = true,
              derived = false,
              onlyShowOnSummary = false,
              None
            ),
            NonEmptyList.of(FormField(FormComponentId("TRES"), "TRES"))
          ))
      )
    )

    val expected =
      <documents>
        <document>
          <header>
            <title>CFJ7CEHP7R</title>
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

  implicit lazy val hc: HeaderCarrier = HeaderCarrier()
}
