package uk.gov.hmrc.gform

import cats.data.NonEmptyList
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.{AvailableLanguages, LangADT, LocalisedString, SmartString}
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PrintSummaryPdf
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.NonRepeatingPage
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{AcknowledgementSection, AuthCtx, ContinueOrDeletePage, DeclarationSection, FormTemplate, FormTemplateRaw, GG, LinkCtx, OnePerUser, Text, TextWithRestrictions, Value}

trait FormTemplatesSupport { this: ITSpec =>
  val basicFormTemplate = Json.parse(
    """
      |{
      |    "_id": "BASIC",
      |    "formName": "Test form name",
      |    "description": "Test form description",
      |    "authConfig": {
      |        "authModule": "anonymous"
      |    },
      |    "destinations": [
      |        {
      |            "id": "HMRCDMS",
      |            "type": "hmrcDms",
      |            "dmsFormId": "TSTHMRCDMS",
      |            "customerId": "${auth.gg}",
      |            "classificationType": "ClassificationType",
      |            "businessArea": "BusinessArea",
      |            "roboticsXml": false
      |        }
      |    ],
      |    "emailTemplateId": "email_template_id",
      |    "sections": [
      |        {
      |            "title": "Page1",
      |            "fields": [
      |                {
      |                    "id": "textField1",
      |                    "type": "text",
      |                    "label": "Text field 1",
      |                    "format": "text"
      |                }
      |            ]
      |        }
      |    ],
      |    "declarationSection": {
      |        "title": "Declaration Page",
      |        "fields": []
      |    },
      |    "acknowledgementSection": {
      |        "title": "Acknowledgement Page",
      |        "fields": []
      |    }
      |}
      |""".stripMargin)

  def postTemplate =
    wsClient
      .url(baseUrl + "/formtemplates")
      .withHttpHeaders("Content-Type" -> "application/json")
      .post(basicFormTemplate.toString())
      .futureValue

  def getAllTemplates = {
    wsClient
      .url(baseUrl + "/formtemplates")
      .withHttpHeaders("Content-Type" -> "application/json")
      .get()
      .futureValue
  }

  def getTemplate = {
    wsClient
      .url(baseUrl + "/formtemplates/BASIC")
      .withHttpHeaders("Content-Type" -> "application/json")
      .get()
      .futureValue
  }

  def getTemplateRaw = {
    wsClient
      .url(baseUrl + "/formtemplates/BASIC/raw")
      .withHttpHeaders("Content-Type" -> "application/json")
      .get()
      .futureValue
  }

  def deleteTemplate = {
    wsClient
      .url(baseUrl + "/formtemplates/BASIC")
      .withHttpHeaders("Content-Type" -> "application/json")
      .delete()
      .futureValue
  }

  def assertBasicFormTemplate(formTemplate: FormTemplate): Unit = {
    formTemplate.formName shouldBe LocalisedString(Map(LangADT.En -> "Test form name"))
    formTemplate.sections.size shouldBe 1
    formTemplate.sections.head shouldBe a[NonRepeatingPage]
    val nonRepeatingPage = formTemplate.sections.head.asInstanceOf[NonRepeatingPage]
    nonRepeatingPage.page.title shouldBe toSmartString("Page1")
    val nonRepeatingPageFields = nonRepeatingPage.page.fields
    nonRepeatingPageFields.size shouldBe 1
    nonRepeatingPageFields.head.id.value shouldBe "textField1"
    //nonRepeatingPageFields.head.mandatory shouldBe true
    nonRepeatingPageFields.head.derived shouldBe false
    nonRepeatingPageFields.head.submissible shouldBe true
    nonRepeatingPageFields.head.onlyShowOnSummary shouldBe false
    nonRepeatingPageFields.head.editable shouldBe true
    nonRepeatingPageFields.head.label shouldBe toSmartString("Text field 1")
    nonRepeatingPageFields.head.validators shouldBe List.empty
    nonRepeatingPageFields.head.`type` shouldBe Text(TextWithRestrictions(0, 1000), Value)
    formTemplate.draftRetrievalMethod shouldBe OnePerUser(ContinueOrDeletePage.Show)
    formTemplate.formCategory shouldBe Default
    formTemplate.languages shouldBe AvailableLanguages.default
    formTemplate.parentFormSubmissionRefs shouldBe List.empty
    formTemplate.summarySection.title shouldBe toSmartString("Check your answers", "Gwiriwch eich atebion")
    formTemplate.summarySection.header shouldBe toSmartString(
      "Make sure the information you have given is correct",
      "Gwnewch yn siŵr bod yr wybodaeth a roddwyd gennych yn gywir")
    formTemplate.summarySection.footer shouldBe SmartString(
      LocalisedString(
        Map(
          LangADT.En -> "##Now send your form\n\nYou need to submit your form on the next screen.\n\nBefore you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)]({0}).",
          LangADT.Cy -> "##Nawr anfonwch eich ffurflen\n\nMae angen i chi gyflwyno’ch ffurflen ar y sgrin nesaf.\n\nCyn i chi wneud hyn gallwch, [argraffu neu gadw copi PDF o’ch atebion (yn agor ffenestr neu dab newydd)]({1})."
        )),
      List(LinkCtx(link = PrintSummaryPdf), LinkCtx(link = PrintSummaryPdf))
    )
    formTemplate.authConfig shouldBe Anonymous
    formTemplate.displayHMRCLogo shouldBe true
    formTemplate.destinations shouldBe a[DestinationList]
    formTemplate.emailTemplateId shouldBe "email_template_id"
    val destinationList = formTemplate.destinations.asInstanceOf[DestinationList]
    destinationList.destinations shouldBe NonEmptyList.one(
      HmrcDms(
        DestinationId("HMRCDMS"),
        "TSTHMRCDMS",
        AuthCtx(GG),
        "ClassificationType",
        "BusinessArea",
        "true",
        true,
        false,
        false,
        None,
        false))
    destinationList.acknowledgementSection shouldBe AcknowledgementSection(
      toSmartString("Acknowledgement Page"),
      None,
      None,
      List.empty,
      true,
      None,
      None,
      true)
    destinationList.declarationSection shouldBe DeclarationSection(
      toSmartString("Declaration Page"),
      None,
      None,
      List.empty)

    val formTemplateRaw = formTemplateRawRepo.find("_id" -> "BASIC").futureValue
    formTemplateRaw shouldBe List(FormTemplateRaw(basicFormTemplate.as[JsObject]))
    ()
  }
}
