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

package uk.gov.hmrc.gform.core

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyText, _ }

class FormValidatorSpec extends Spec {

  def getMandatoryFieldValue(id: String) =
    FormComponent(
      id = FormComponentId(id),
      `type` = Text(AnyText, Value),
      label = "",
      helpText = None,
      None,
      validIf = None,
      editable = true,
      mandatory = true,
      submissible = true,
      derived = false,
      errorMessage = None
    )

  def getAddressFieldValue(id: String) =
    FormComponent(
      id = FormComponentId(id),
      `type` = Address(international = false),
      label = "",
      helpText = None,
      None,
      validIf = None,
      editable = true,
      mandatory = false,
      submissible = true,
      derived = false,
      errorMessage = None
    )

  "Validation of form fields" should "succeed" in {

    val formFields =
      List(
        FormField(FormComponentId("iptRegNum"), "12AB3456780"),
        FormField(FormComponentId("firstName"), "John"),
        FormField(FormComponentId("lastName"), "Doe"),
        FormField(FormComponentId("telephoneNumber"), "+44 (01273) 123456"),
        FormField(FormComponentId("nameOfBusiness"), "Acme Widgets Ltd.")
      )

    val section = Section(
      title = "",
      description = None,
      None,
      shortName = None,
      None,
      None,
      None,
      None,
      fields =
        List("iptRegNum", "firstName", "lastName", "telephoneNumber", "nameOfBusiness").map(getMandatoryFieldValue),
      None,
      None
    )

    val res = FormValidator.validate(formFields, section)

    res.right.value should be(())

  }

  it should "succeed with address component" in {

    val formFields =
      List(
        FormField(FormComponentId("iptRegNum"), "12AB3456780"),
        FormField(FormComponentId("firstName"), "John"),
        FormField(FormComponentId("lastName"), "Doe"),
        FormField(FormComponentId("telephoneNumber"), "+44 (01273) 123456"),
        FormField(FormComponentId("nameOfBusiness"), "Acme Widgets Ltd."),
        FormField(FormComponentId("homeAddress-street1"), "1"),
        FormField(FormComponentId("homeAddress-street2"), "2"),
        FormField(FormComponentId("homeAddress-street3"), "3"),
        FormField(FormComponentId("homeAddress-street4"), "4"),
        FormField(FormComponentId("homeAddress-postcode"), "6"),
        FormField(FormComponentId("homeAddress-country"), "7")
      )

    val section = Section(
      "",
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      fields = getAddressFieldValue("homeAddress") :: List(
        "iptRegNum",
        "firstName",
        "lastName",
        "telephoneNumber",
        "nameOfBusiness").map(getMandatoryFieldValue),
      None,
      None
    )

    val res = FormValidator.validate(formFields, section)

    res.right.value should be(())

  }

  it should "fail if form contains field not defined in template fields" in {

    val formFields =
      List(FormField(FormComponentId("iptRegNum"), "12AB3456780"))

    val section = Section("", None, None, None, None, None, None, None, fields = List.empty[FormComponent], None, None)

    val res = FormValidator.validate(formFields, section)

    res.left.value should be(UnexpectedState("Field iptRegNum is not part of the template"))

  }

  it should "fail if form don't contains field which is mandatory" in {

    val formFields = List.empty[FormField]

    val section =
      Section(
        "",
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        fields = List("iptRegNum").map(getMandatoryFieldValue),
        None,
        None)

    val res = FormValidator.validate(formFields, section)

    res.left.value should be(UnexpectedState("Required fields iptRegNum are missing in form submission."))

  }
}
