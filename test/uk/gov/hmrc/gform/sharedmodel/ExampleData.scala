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

package uk.gov.hmrc.gform.sharedmodel

import java.time.LocalDateTime

import uk.gov.hmrc.gform.fileupload.RouteEnvelopeRequest
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyText, _ }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionRef }

import scala.collection.immutable.List

object ExampleData extends ExampleData

trait ExampleData
  extends ExampleFormTemplate
  with ExampleFieldId
  with ExampleFieldValue
  with ExampleFormField
  with ExampleSection
  with ExampleForm
  with ExampleAuthConfig
  with ExampleSubmission
  with ExampleRouteEnvelopeRequest

trait ExampleAuthConfig {

  def authConfigModule = AuthConfigModule("TESTAuthConfigModule")

  def dmsSubmission = DmsSubmission("nino", "BT-NRU-Environmental", "FinanceOpsCorpT")

  def regimeId = RegimeId("TestRegimeId")

  def authConfig = AuthConfig(authConfigModule, None, regimeId)
}

trait ExampleFieldId {

  def `fieldId - facePhoto` = FieldId("facePhoto")
  def `fieldId - surname` = FieldId("surname")
  def `fieldId - firstName` = FieldId("firstName")
  def `fieldId - iptRegNum` = FieldId("iptRegNum")
  def `fieldId - businessName` = FieldId("nameOfBusiness")
  def `fieldId - startDate` = FieldId("startDate")

  def `fieldId - startDate-year` = FieldId("startDate-year")
  def `fieldId - startDate-day` = FieldId("startDate-day")
  def `fieldId - startDate-month` = FieldId("startDate-month")

}

trait ExampleFieldValue { dependecies: ExampleFieldId =>

  def `fieldValue - facePhoto` = FieldValue(
    `fieldId - facePhoto`,
    FileUpload(), label = "Attach evidence of your smile", helpText = None, None, mandatory = true, editable = true, submissible = true,
    None,
    None
  )

  def `fieldValue - firstName` = FieldValue(
    `fieldId - firstName`,
    Text(AnyText, Constant("any text"), total = false), "First Name", None, None, mandatory = true, editable = true, submissible = true,
    None,
    None
  )

  def `fieldValue - surname` = FieldValue(
    `fieldId - surname`,
    Text(AnyText, Constant("any text"), total = false), "Last Name", None, None, mandatory = true, editable = true, submissible = true,
    None,
    None
  )

  def `fieldValue - iptRegNum` = FieldValue(
    `fieldId - iptRegNum`,
    Text(AnyText, Constant("any text"), total = false), "Insurance Premium Tax (IPT) number", None, None, mandatory = true, editable = true, submissible = true,
    None,
    None
  )

  def `fieldValue - businessName` = FieldValue(
    `fieldId - businessName`,
    Text(AnyText, Constant("any text"), total = false), "Name of business", None, None, mandatory = true, editable = true, submissible = true,
    None,
    None
  )

  def `fieldValue - startDate` = FieldValue(
    `fieldId - startDate`,
    Date(AnyDate, Offset(0), None), "Your Start Date", None, None, true, true, true,
    None,
    None
  )

  def `group - type` = Group(
    fields = List(`fieldValue - firstName`),
    orientation = Horizontal,
    repeatsMax = None,
    repeatsMin = None,
    repeatLabel = None,
    repeatAddAnotherText = None
  )

  def `fieldValue - group` = FieldValue(
    id = FieldId("GroupFieldValueId"),
    `type` = `group - type`,
    label = "group FieldValue label",
    helpText = None,
    shortName = None,
    mandatory = true,
    editable = false,
    submissible = true,
    errorMessage = None
  )
}

trait ExampleSection { dependecies: ExampleFieldId with ExampleFieldValue =>

  def `section - about you` = Section(
    "About you",
    None, None, None, None, None,
    List(
      `fieldValue - firstName`,
      `fieldValue - surname`,
      `fieldValue - facePhoto`
    )
  )

  def `section - businessDetails` = Section(
    "Business details",
    None, None, None, None, None,
    List(
      `fieldValue - businessName`,
      `fieldValue - startDate`,
      `fieldValue - iptRegNum`
    )
  )

  def `repeating section` = Section(
    "Repeating section",
    None, None, None,
    repeatsMax = Some(TextExpression(FormCtx(`fieldId - firstName`.value))),
    repeatsMin = Some(TextExpression(FormCtx(`fieldId - firstName`.value))),
    List(
      `fieldValue - surname`
    )
  )

  def `section - group` = `section - about you`.copy(fields = List(`fieldValue - group`))

  def allSections = List(
    `section - about you`,
    `section - businessDetails`
  )
}

trait ExampleFormTemplate { dependsOn: ExampleAuthConfig with ExampleSection with ExampleFieldId =>

  def formTemplateId = FormTemplateId("AAA999")

  def formName = "AAA999 dev test template"

  def formDescription = "Fill in your insurance premium tax return form online | Llenwch eich ffurflen dreth premiwm yswiriant ar-lein"

  def submtSuccessUrl = """http://success.com"""

  def submitErrorUrl = """http://imsorry.com"""

  def formTemplate = FormTemplate(
    _id = formTemplateId,
    formName = formName,
    description = formDescription,
    dmsSubmission = dmsSubmission,
    authConfig = authConfig,
    submitSuccessUrl = submtSuccessUrl,
    submitErrorUrl = submitErrorUrl,
    sections = allSections
  )
}

trait ExampleFormField { dependsOn: ExampleFormTemplate with ExampleFieldId =>

  def `formField - facePhoto` = FormField(`fieldId - facePhoto`, "face-photo.jpg")
  def `formField - firstName` = FormField(`fieldId - firstName`, "James")
  def `formField - surname` = FormField(`fieldId - surname`, "Bond")
  def `formField - iptRegNum` = FormField(`fieldId - iptRegNum`, "666CasinoRoyale")
  def `formField - businessName` = FormField(`fieldId - businessName`, "Quantum of Solace")
  def `formField - startDateDay` = FormField(`fieldId - startDate-day`, "11")
  def `formField - startDateMonth` = FormField(`fieldId - startDate-month`, "10")
  def `formField - startDateYear` = FormField(`fieldId - startDate-year`, "2008")

  def data = Map(
    `fieldId - facePhoto` -> `formField - facePhoto`,
    `fieldId - firstName` -> `formField - firstName`,
    `fieldId - surname` -> `formField - surname`,
    `fieldId - iptRegNum` -> `formField - iptRegNum`,
    `fieldId - startDate-year` -> `formField - startDateYear`,
    `fieldId - startDate-month` -> `formField - startDateMonth`,
    `fieldId - startDate-day` -> `formField - startDateDay`,
    `fieldId - businessName` -> `formField - businessName`
  )
}

trait ExampleForm { dependsOn: ExampleFormField with ExampleFormTemplate =>

  def userId = UserId("James007")

  def formId = FormId(userId, formTemplateId)

  def formFields: Seq[FormField] = data.values.toSeq

  def formData = FormData(formFields)

  def formDataNil = FormData(fields = Nil)

  def envelopeId = EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307")

  def form = Form(
    formId,
    envelopeId,
    userId,
    formTemplateId,
    None,
    formData
  )

}

trait ExampleSubmission { dependsOn: ExampleForm with ExampleFormTemplate =>

  def submittedDate = LocalDateTime.parse("2007-12-03T10:15:30")

  def submissionRef = SubmissionRef("DMS")

  def dmsMetaData = DmsMetaData(
    formTemplateId
  )

  def submission = Submission(
    formId,
    submittedDate,
    submissionRef,
    envelopeId,
    dmsMetaData
  )

}

trait ExampleRouteEnvelopeRequest { dependsOn: ExampleForm =>

  def application: String = "dfs"
  def destination: String = "DMS"
  def routeEnvelopeRequest = RouteEnvelopeRequest(envelopeId, application, destination)

}