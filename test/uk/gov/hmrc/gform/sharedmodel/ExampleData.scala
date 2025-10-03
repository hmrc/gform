/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.objectstore.RouteEnvelopeRequest
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AccessibilityUrl, AcknowledgementSection, Anonymous, AnyDate, ChatRoomId, Constant, ContinueOrDeletePage, Date, DeclarationSection, Default, EmailParameter, EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable, Expr, FileUpload, FormComponent, FormComponentId, FormCtx, FormKind, FormTemplate, FormTemplateId, FormTemplateVersion, Group, IncludeIf, InformationMessage, KeyDisplayWidth, LayoutDisplayWidth, Mandatory, NoFormat, Number, Offset, OnePerUser, Page, PdfCtx, RegimeId, ResearchBanner, Section, ServiceId, ServiceStartPageUrl, ShortText, SummarySection, TemplateName, Text, UserResearchUrl, Value, WebChat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.{ DataStore, HmrcDms }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DataOutputFormat, DestinationId, DestinationIncludeIf, InstructionPdfFields, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }

import java.time.{ Instant, LocalDateTime }

object ExampleData extends ExampleData

trait ExampleData
    extends ExampleFormTemplate with ExampleFieldId with ExampleFieldValue with ExampleFormField with ExampleSection
    with ExampleForm with ExampleAuthConfig with ExampleSubmission with ExampleRouteEnvelopeRequest

trait ExampleAuthConfig extends DestinationGen {

  val hmrcDms = HmrcDms(
    DestinationId("TestHmrcDmsId"),
    "TestHmrcDmsFormId",
    Constant("TestHmrcDmsCustomerId"),
    "TestHmrcDmsClassificationType",
    "TestHmrcDmsBusinessArea",
    DestinationIncludeIf.HandlebarValue(""),
    true,
    Some(DataOutputFormat.XML),
    true,
    Some(true),
    Some(InstructionPdfFields.Ordered),
    None,
    None,
    TemplateType.XML
  )

  val dataStore = DataStore(
    DestinationId("TestDataStoreId"),
    SdesDestination.DataStore,
    DestinationIncludeIf.HandlebarValue(""),
    true,
    FormId("TestHmrcDmsFormId"),
    "1",
    Constant("TestTaxpayerId"),
    "TestRegime",
    true,
    false,
    false,
    None,
    None,
    false,
    None,
    None
  )

  private def buildFormComponent(name: String, expr: Expr) =
    FormComponent(
      FormComponentId(name),
      Text(ShortText.default, expr),
      toSmartString(name),
      false,
      None,
      None,
      None,
      None,
      Mandatory.True,
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
      Some(toSmartString("ack section with email param field")),
      None,
      None,
      ackFormComponent,
      true,
      Some(
        PdfCtx(
          Some(toSmartString("It's a Acknowledgement Section Pdf header.")),
          Some(toSmartString("It's a Acknowledgement Section Pdf footer.")),
          None,
          None
        )
      ),
      Some(
        PdfCtx(
          Some(toSmartString("It's a Acknowledgement Section Instruction Pdf header.")),
          Some(toSmartString("It's a Acknowledgement Section Instruction Pdf footer.")),
          None,
          None
        )
      ),
      true,
      None,
      true
    )

  val decFormComponent = List(buildFormComponent("fieldInDeclarationSections", Value))

  val decSection =
    DeclarationSection(
      toSmartString("declaration section with email param field"),
      None,
      None,
      None,
      None,
      Some(toSmartString("Declaration section with continueLabel")),
      decFormComponent,
      None
    )

  val decSectionWithGroupComponent =
    DeclarationSection(
      toSmartString("Declaration section with Group Component"),
      None,
      None,
      None,
      None,
      Some(toSmartString("Declaration section with Group Component")),
      List(
        FormComponent(
          FormComponentId("groupComponentInDeclarationSection"),
          Group(List(buildFormComponent("fieldInDeclarationSections", Value)), None, None, None, None),
          toSmartString("groupComponentInDeclarationSection"),
          false,
          None,
          None,
          None,
          None,
          Mandatory.True,
          false,
          true,
          false,
          false,
          None,
          None
        )
      ),
      None
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
      FileUpload(None, None),
      label = toSmartString("Attach evidence of your smile"),
      isPageHeading = false,
      helpText = None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = Mandatory.True,
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
      false,
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = Mandatory.True,
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
      false,
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = Mandatory.True,
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
      false,
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = Mandatory.True,
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
      false,
      None,
      None,
      includeIf = None,
      validIf = None,
      mandatory = Mandatory.True,
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
      false,
      None,
      None,
      includeIf = None,
      validIf = None,
      Mandatory.True,
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
      false,
      None,
      None,
      includeIf = None,
      validIf = None,
      Mandatory.False,
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
      isPageHeading = false,
      helpText = None,
      shortName = None,
      includeIf = None,
      validIf = None,
      mandatory = Mandatory.True,
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
      false,
      None,
      None,
      includeIf = None,
      validIf = None,
      Mandatory.True,
      true,
      true,
      derived = false,
      onlyShowOnSummary = false,
      None
    )

}

trait ExampleSection { dependecies: ExampleFieldId with ExampleFieldValue =>
  def nonRepeatingPageSection(
    title: String = "About you",
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
        fields,
        None,
        None,
        None,
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
        List(`fieldValue - surname`),
        None,
        None,
        None,
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

  def allSections = FormKind.Classic(List(`section - about you`, `section - businessDetails`))
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

  def emailTemplateId = Some(LocalisedEmailTemplateId("test-email-template-id", None))

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
      Some(toSmartString("Acknowledgement Page")),
      Some(toSmartString("this page is to acknowledge submission")),
      Some(toSmartString("shortName for acknowledgement")),
      List(`fieldValue - info`),
      true,
      None,
      None,
      true,
      None,
      true
    )

  def declarationSection =
    DeclarationSection(
      toSmartString("Declaration"),
      None,
      None,
      None,
      None,
      Some(toSmartString("ContinueLabel")),
      Nil,
      None
    )

  def summarySection = SummarySection(
    toSmartString("Title"),
    Some(toSmartString("Caption")),
    toSmartString("Header"),
    toSmartString("Footer"),
    Some(toSmartString("ContinueLabel")),
    None,
    LayoutDisplayWidth.M,
    KeyDisplayWidth.S,
    None,
    None,
    None,
    None
  )

  def userResearchUrl = UserResearchUrl("https://test.service.gov.uk")

  def accessibilityUrl = AccessibilityUrl(formTemplateId.value)

  def serviceStartPageUrl = ServiceStartPageUrl("https://startpage.service.gov.uk")

  def formTemplate = FormTemplate(
    formTemplateId,
    formTemplateId,
    FormTemplateVersion(1),
    legacyFormIds,
    formName,
    Some(ResearchBanner),
    Default,
    OnePerUser(ContinueOrDeletePage.Show),
    None,
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
    None,
    true,
    FileInfoConfig.allAllowedFileTypes,
    None,
    Some(userResearchUrl),
    None,
    None,
    Some(accessibilityUrl),
    None,
    None,
    None,
    None,
    None,
    false,
    false,
    Some(serviceStartPageUrl),
    true,
    None,
    false
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
      VisitIndex.empty(FormKind.Classic(Nil)),
      ThirdPartyData.empty,
      envelopeExpiryDate,
      FormComponentIdToFileIdMapping.empty,
      TaskIdTaskStatusMapping.empty,
      Instant.now
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
