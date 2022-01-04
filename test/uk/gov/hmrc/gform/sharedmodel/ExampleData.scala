/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.fileupload.RouteEnvelopeRequest
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }

import scala.collection.immutable.List

object ExampleData extends ExampleData

trait ExampleData
    extends ExampleFormTemplate with ExampleFieldId with ExampleFieldValue with ExampleFormField with ExampleValidator
    with ExampleSection with ExampleForm with ExampleAuthConfig with ExampleSubmission with ExampleRouteEnvelopeRequest

trait ExampleAuthConfig extends DestinationGen {

  val hmrcDms = HmrcDms(
    DestinationId("TestHmrcDmsId"),
    "TestHmrcDmsFormId",
    Constant("TestHmrcDmsCustomerId"),
    "TestHmrcDmsClassificationType",
    "TestHmrcDmsBusinessArea",
    "",
    true,
    true,
    true,
    Some(true),
    true
  )

  private def buildFormComponent(name: String, expr: Expr) =
    FormComponent(
      FormComponentId(name),
      Text(ShortText.default, expr),
      toSmartString(name),
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  val ackFormComponent = List(buildFormComponent("fieldInAcknowledgementSections", Value))

  val ackSection =
    AcknowledgementSection(
      toSmartString("ack section with email param field"),
      None,
      None,
      ackFormComponent,
      true,
      Some(
        AcknowledgementSectionPdf(
          Some(toSmartString("It's a Acknowledgement Section Pdf header.")),
          Some(toSmartString("It's a Acknowledgement Section Pdf footer."))
        )
      ),
      Some(
        AcknowledgementSectionPdf(
          Some(toSmartString("It's a Acknowledgement Section Instruction Pdf header.")),
          Some(toSmartString("It's a Acknowledgement Section Instruction Pdf footer."))
        )
      ),
      true
    )

  val decFormComponent = List(buildFormComponent("fieldInDeclarationSections", Value))

  val decSection =
    DeclarationSection(
      toSmartString("declaration section with email param field"),
      None,
      None,
      None,
      Some(toSmartString("Declaration section with continueLabel")),
      decFormComponent
    )

  val decSectionWithGroupComponent =
    DeclarationSection(
      toSmartString("Declaration section with Group Component"),
      None,
      None,
      None,
      Some(toSmartString("Declaration section with Group Component")),
      List(
        FormComponent(
          FormComponentId("groupComponentInDeclarationSection"),
          Group(List(buildFormComponent("fieldInDeclarationSections", Value)), None, None, None, None),
          toSmartString("groupComponentInDeclarationSection"),
          None,
          None,
          None,
          None,
          true,
          false,
          true,
          false,
          false,
          None,
          None
        )
      )
    )

  def destinationList = DestinationList(NonEmptyList.of(hmrcDms), ackSection, Some(decSection))

  def destinationListWithGroupComponentInDecSection =
    DestinationList(NonEmptyList.of(hmrcDms), ackSection, Some(decSectionWithGroupComponent))

  def serviceId = ServiceId("TestServiceId")

  def regimeId = RegimeId("TestRegimeId")

  def authConfig = Anonymous
}

trait ExampleFieldId {

  def `fieldId - facePhoto` = FormComponentId("facePhoto")
  def `fieldId - surname` = FormComponentId("surname")
  def `fieldId - firstName` = FormComponentId("firstName")
  def `fieldId - iptRegNum` = FormComponentId("iptRegNum")
  def `fieldId - businessName` = FormComponentId("nameOfBusiness")
  def `fieldId - startDate` = FormComponentId("startDate")

  def `fieldId - startDate-year` = FormComponentId("startDate-year")
  def `fieldId - startDate-day` = FormComponentId("startDate-day")
  def `fieldId - startDate-month` = FormComponentId("startDate-month")

  def `fieldId - number` = FormComponentId("number")

}

trait ExampleFieldValue { dependecies: ExampleFieldId =>

  def `fieldValue - facePhoto` =
    FormComponent(
      `fieldId - facePhoto`,
      FileUpload(FileUploadProvider.FileUploadFrontend),
      label = toSmartString("Attach evidence of your smile"),
      helpText = None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      None
    )

  def `fieldValue - firstName` =
    FormComponent(
      `fieldId - firstName`,
      Text(ShortText.default, Constant("any text")),
      toSmartString("First Name"),
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      None
    )

  def `fieldValue - surname` =
    FormComponent(
      `fieldId - surname`,
      Text(ShortText.default, Constant("any text")),
      toSmartString("Last Name"),
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      None
    )

  def `fieldValue - iptRegNum` =
    FormComponent(
      `fieldId - iptRegNum`,
      Text(ShortText.default, Constant("any text")),
      toSmartString("Insurance Premium Tax (IPT) number"),
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      None
    )

  def `fieldValue - businessName` =
    FormComponent(
      `fieldId - businessName`,
      Text(ShortText.default, Constant("any text")),
      toSmartString("Name of business"),
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      None
    )

  def `fieldValue - startDate` =
    FormComponent(
      `fieldId - startDate`,
      Date(AnyDate, Offset(0), None),
      toSmartString("Your Start Date"),
      None,
      None,
      includeIf = None,
      validIf = None,
      true,
      true,
      true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      None
    )

  def `fieldValue - info` =
    FormComponent(
      `fieldId - businessName`,
      InformationMessage(NoFormat, toSmartString("some text")),
      toSmartString("someLabel"),
      None,
      None,
      includeIf = None,
      validIf = None,
      false,
      false,
      false,
      derived = false,
      onlyShowOnSummary = false,
      None
    )

  def `group - type` =
    Group(
      fields = List(`fieldValue - firstName`),
      repeatsMax = None,
      repeatsMin = None,
      repeatLabel = None,
      repeatAddAnotherText = None
    )

  def `fieldValue - group` =
    FormComponent(
      id = FormComponentId("GroupFieldValueId"),
      `type` = `group - type`,
      label = toSmartString("group FieldValue label"),
      helpText = None,
      shortName = None,
      includeIf = None,
      validIf = None,
      mandatory = true,
      editable = false,
      submissible = true,
      derived = false,
      errorMessage = None
    )

  def `fieldValue - number` =
    FormComponent(
      `fieldId - number`,
      Text(Number(), Value),
      toSmartString("sample label"),
      None,
      None,
      includeIf = None,
      validIf = None,
      true,
      true,
      true,
      derived = false,
      onlyShowOnSummary = false,
      None
    )

}

trait ExampleValidator {
  def defaultValidator = hMRCUTRPostcodeCheckValidator
  def hMRCUTRPostcodeCheckValidator =
    HmrcRosmRegistrationCheckValidator(
      toSmartString("The UTR could not be foundor the postcode did not match. | <Welsh...>"),
      "ITSA",
      FormCtx(FormComponentId("utrToCheck")),
      FormCtx(FormComponentId("postcodeToCheck"))
    )
}

trait ExampleSection { dependecies: ExampleFieldId with ExampleFieldValue with ExampleValidator =>
  def nonRepeatingPageSection(
    title: String = "About you",
    validators: Option[Validator] = Some(defaultValidator),
    fields: List[FormComponent] = List(`fieldValue - firstName`, `fieldValue - surname`, `fieldValue - facePhoto`),
    includeIf: Option[IncludeIf] = None
  ) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(title),
        None,
        None,
        None,
        None,
        None,
        includeIf,
        validators,
        fields,
        None,
        None,
        None,
        None,
        None,
        None
      )
    )

  def `section - about you` =
    nonRepeatingPageSection()

  def `section - businessDetails` =
    nonRepeatingPageSection(
      title = "Business details",
      validators = None,
      fields = List(`fieldValue - businessName`, `fieldValue - startDate`, `fieldValue - iptRegNum`)
    )

  def `repeating section` =
    Section.RepeatingPage(
      Page(
        toSmartString("Repeating section"),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        List(`fieldValue - surname`),
        None,
        None,
        None,
        None,
        None,
        None
      ),
      repeats = FormCtx(`fieldId - firstName`)
    )

  def `section - group` =
    nonRepeatingPageSection(
      fields = List(`fieldValue - group`)
    )

  def allSections = List(`section - about you`, `section - businessDetails`)
}

trait ExampleFormTemplate {
  dependsOn: ExampleAuthConfig with ExampleSection with ExampleFieldId with ExampleFieldValue =>

  def formTemplateId = FormTemplateId("aaa999")

  def legacyFormIds =
    Some(
      NonEmptyList.of(
        FormTemplateId("aaa100")
      )
    )

  def formName = toLocalisedString("aaa999 dev test template")

  def emailTemplateId = LocalisedEmailTemplateId("test-email-template-id", None)

  def emailParameters =
    Some(
      NonEmptyList.of(
        EmailParameter("fullName", FormCtx(FormComponentId("directorFullName"))),
        EmailParameter("email", FormCtx(FormComponentId("directorEmail")))
      )
    )

  def webChat = Some(WebChat(ChatRoomId("test"), TemplateName("hmrc7")))

  def acknowledgementSection =
    AcknowledgementSection(
      toSmartString("Acknowledgement Page"),
      Some(toSmartString("this page is to acknowledge submission")),
      Some(toSmartString("shortName for acknowledgement")),
      List(`fieldValue - info`),
      true,
      None,
      None,
      true
    )

  def declarationSection =
    DeclarationSection(
      toSmartString("Declaration"),
      None,
      None,
      None,
      Some(toSmartString("ContinueLabel")),
      Nil
    )

  def summarySection = SummarySection(
    toSmartString("Title"),
    toSmartString("Header"),
    toSmartString("Footer"),
    Some(toSmartString("ContinueLabel"))
  )

  def userResearchUrl = UserResearchUrl("https://test.service.gov.uk")

  def accessibilityUrl = AccessibilityUrl(formTemplateId.value)

  def formTemplate = FormTemplate(
    formTemplateId,
    formTemplateId,
    None,
    legacyFormIds,
    formName,
    Some(ResearchBanner),
    Default,
    OnePerUser(ContinueOrDeletePage.Show),
    destinationList,
    authConfig,
    emailTemplateId,
    emailParameters,
    webChat,
    allSections,
    Nil,
    AvailableLanguages.default,
    None,
    summarySection,
    true,
    Some(userResearchUrl),
    None,
    None,
    Some(accessibilityUrl)
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
  def `formField - number` = FormField(`fieldId - number`, "£1,234")

  def data =
    Map(
      `fieldId - facePhoto`       -> `formField - facePhoto`,
      `fieldId - firstName`       -> `formField - firstName`,
      `fieldId - surname`         -> `formField - surname`,
      `fieldId - iptRegNum`       -> `formField - iptRegNum`,
      `fieldId - startDate-year`  -> `formField - startDateYear`,
      `fieldId - startDate-month` -> `formField - startDateMonth`,
      `fieldId - startDate-day`   -> `formField - startDateDay`,
      `fieldId - businessName`    -> `formField - businessName`,
      `fieldId - number`          -> `formField - number`
    )
}

trait ExampleForm { dependsOn: ExampleFormField with ExampleFormTemplate =>

  def userId = UserId("James007")

  def formId = FormId(userId, formTemplateId)

  def formFields: Seq[FormField] = data.values.toSeq

  def formData = FormData(formFields)

  def formDataNil = FormData(fields = Nil)

  def envelopeId = EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307")

  def obligations = None

  def formEmailParameters = EmailParametersRecalculated(Map.empty[EmailTemplateVariable, EmailParameterValue])

  val envelopeExpiryDate = Some(EnvelopeExpiryDate(LocalDateTime.now.plusDays(1)))

  def form =
    Form(
      formId,
      envelopeId,
      userId,
      formTemplateId,
      None,
      formData,
      InProgress,
      VisitIndex.empty,
      ThirdPartyData.empty,
      envelopeExpiryDate,
      FormComponentIdToFileIdMapping.empty
    )

}

trait ExampleSubmission { dependsOn: ExampleForm with ExampleFormTemplate =>

  def submittedDate = LocalDateTime.parse("2007-12-03T10:15:30")

  def submissionRef = SubmissionRef("DMS")

  def dmsMetaData = DmsMetaData(formTemplateId, "TESTNINO")

  def submission =
    Submission(SubmissionId(formId, envelopeId), submittedDate, submissionRef, envelopeId, 0, dmsMetaData)

}

trait ExampleRouteEnvelopeRequest { dependsOn: ExampleForm =>

  def application: String = "dfs"
  def destination: String = "DMS"
  def routeEnvelopeRequest = RouteEnvelopeRequest(envelopeId, application, destination)

}
