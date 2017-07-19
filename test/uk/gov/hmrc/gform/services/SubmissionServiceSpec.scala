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

import java.time.LocalDateTime

import org.scalatest.time.{ Millis, Span }
import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.Json
import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.connectors.PDFGeneratorConnector
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses._
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.collection.immutable.List
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

class SubmissionServiceSpec extends Spec with TypeclassFixtures {

  implicit override val patienceConfig = PatienceConfig(timeout = scaled(Span(15000, Millis)), interval = scaled(Span(300, Millis)))

  val form = Form(FormId("form-id"), FormData(UserId("TESTID"), FormTypeId("form-type-id"), Version("1.0.0"), "UTF-8", List(FormField(FieldId("firstName"), "Joe"), FormField(FieldId("lastName"), "Doe"))), EnvelopeId("123"))

  val plainFormTemplate = FormTemplate(Some("schemaId"), FormTypeId("IPT100"), "Insurance Premium Tax Return", Version("version"), "description", "characterSet", DmsSubmission("nino", "BT-NRU-Environmental", "FinanceOpsCorpT"), "submitSuccessUrl", "submitErrorUrl", List.empty[Section])

  val yourDetailsSection = Section(
    "Your details",
    None, None, None,
    List(
      FieldValue(FieldId("firstName"), Text(AnyText, Constant(""), total = false), "Your first name", None, None, mandatory = true, editable = true, submissible = true),
      FieldValue(FieldId("lastName"), Text(AnyText, Constant(""), total = false), "Your last name", None, None, mandatory = true, editable = true, submissible = true)
    )
  )

  val formTemplateWithOneSection = plainFormTemplate.copy(sections = List(yourDetailsSection))

  "SubmissionService submission" should "submit form" in {

    val localDataTime = LocalDateTime.of(2017, 1, 31, 13, 53, 45)
    implicit val now = Now(localDataTime)
    implicit val rnd = Rnd(new Random(123))

    val findOneCheck = mock[FindOneCheck]
    val insertCheck = mock[InsertCheck]
    val postCheck = mock[PostCheck]

    implicit val findOneFormTemplate: FindOne[FormTemplate] = FindOneTC
      .response(Some(formTemplateWithOneSection))
      .callCheck(findOneCheck)
      .noChecks

    implicit val findOneForm: FindOne[Form] = FindOneTC
      .response(Some(form))
      .callCheck(findOneCheck)
      .noChecks

    implicit val insertSubmission: Insert[Submission] = InsertTC
      .response(Right(Success))
      .callCheck(insertCheck)
      .withChecks {
        case (selector, submission) =>

          selector should be(Json.obj())

          inside(submission) {
            case Submission(localDataTime, submissionRef, formId, envelopeId, dmsMetaData) =>
              localDataTime should be(localDataTime)
              submissionRef.value should be("0OU-RDFS-NRN")
              formId.value should be("form-id")
              envelopeId.value should be("123")
              dmsMetaData should be(DmsMetaData(FormTypeId("form-type-id")))
          }
      }

    implicit val postUploadFile: Post[UploadFile, HttpResponse] = PostTC
      .response(HttpResponse(responseStatus = 200))
      .callCheck(postCheck)
      .withChecks { uploadFile =>
        inside(uploadFile) {
          case UploadFile(envelopeId, fileId, fileName, contentType, body) =>
            envelopeId.value should be("123")
            fileId.value should (be("pdf") or be("xmlDocument"))
            fileName should (be("0OU-RDFS-NRN-20170131-iform.pdf") or be("0OU-RDFS-NRN-20170131-metadata.xml"))
            contentType should (be("application/pdf") or be("application/xml; charset=UTF-8"))
        }
      }

    implicit val postRouteEnvelopeRequest: Post[RouteEnvelopeRequest, HttpResponse] = PostTC
      .response(HttpResponse(responseStatus = 200))
      .callCheck(postCheck)
      .withChecks { routeEnvelopeRequest =>
        inside(routeEnvelopeRequest) {
          case RouteEnvelopeRequest(envelopeId, application, destination) =>
            envelopeId.value should be("123")
            application should be("dfs")
            destination should be("DMS")
        }
      }

    (findOneCheck.call _).expects().twice
    (insertCheck.call _).expects().once
    (postCheck.call _).expects().repeat(3)

    val testSubmissionService = new SubmissionService {
      override def htmlGenerator: HtmlGeneratorService = new HtmlGeneratorService {
        override def generateDocumentHTML(sectionFormFields: List[SectionFormField], formName: String): String = {
          "HELLO"
        }
      }
      override def pdfGenerator: PDFGeneratorService = new PDFGeneratorService {
        override def pdfConnector: PDFGeneratorConnector = ???

        override def generatePDF(html: String)(implicit hc: HeaderCarrier): Future[Array[Byte]] = {
          Future.successful("HELLO".getBytes)
        }
      }
    }
    val res = testSubmissionService.submission(FormId("form-id"))

    futureResult(res.value).right.value should be("http://localhost:8898/file-transfer/envelopes/123")
  }

  "SubmissionService.getSectionFormFields" should "find repeating group fields" in {
    val formFields = Seq[FormField](
      FormField(FieldId("UNO"), "UNO"),
      FormField(FieldId("1_UNO"), "1_UNO"),
      FormField(FieldId("2_UNO"), "2_UNO"),
      FormField(FieldId("3_UNO"), "3_UNO"),
      FormField(FieldId("4_UNO"), "4_UNO"),
      FormField(FieldId("DOS"), "DOS"),
      FormField(FieldId("1_DOS"), "1_DOS"),
      FormField(FieldId("2_DOS"), "2_DOS"),
      FormField(FieldId("3_DOS"), "3_DOS"),
      FormField(FieldId("4_DOS"), "4_DOS")
    )
    val formData = FormData(UserId("TESTID"), FormTypeId("JustAFormTypeId"), Version("-11"), "UTF-16", formFields)
    val form = Form(FormId("MIO"), formData, EnvelopeId(""))

    val textFieldUno = FieldValue(
      id = FieldId("UNO"),
      `type` = Text(AnyText, Constant("UNO"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true
    )

    val textFieldDos = textFieldUno.copy(id = FieldId("DOS"), `type` = Text(AnyText, Constant("DOS"), false))

    val group = Group(
      fields = List(textFieldUno, textFieldDos),
      orientation = Horizontal,
      repeatsMax = Some(2),
      repeatsMin = Some(1),
      repeatLabel = Some("repeat label"),
      repeatAddAnotherText = Some("add group button label")
    )

    val groupFieldValue = FieldValue(
      id = FieldId("GroupFieldValueId"),
      `type` = group,
      label = "group FieldValue label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = false,
      submissible = true
    )

    val section = Section(
      title = "Section title",
      description = None,
      shortName = None,
      includeIf = None,
      fields = List(groupFieldValue)
    )

    val formTemplate = FormTemplate(
      schemaId = Some("2.0"),
      formTypeId = FormTypeId("JustAFormTypeId"),
      formName = "formName",
      version = Version("-11"),
      description = "formTemplateDescription",
      characterSet = "UTF-16",
      dmsSubmission = DmsSubmission("customerId", "classificationType", "businessArea"),
      submitSuccessUrl = "http://somwehere-nice.net",
      submitErrorUrl = "http://somwehere-nasty.net",
      sections = List(section)
    )

    val expectedResult = List(
      SectionFormField(
        "Section title",
        List(
          (
            List(FormField(FieldId("UNO"), "UNO")),
            FieldValue(FieldId("UNO"), Text(AnyText, Constant("UNO"), false), "Editable text label", None, None, true, true, true)
          ),
          (
            List(FormField(FieldId("DOS"), "DOS")),
            FieldValue(FieldId("DOS"), Text(AnyText, Constant("DOS"), false), "Editable text label", None, None, true, true, true)
          ),
          (
            List(FormField(FieldId("1_UNO"), "1_UNO")),
            FieldValue(FieldId("1_UNO"), Text(AnyText, Constant("UNO"), false), "Editable text label", None, None, true, true, true)
          ),
          (
            List(FormField(FieldId("1_DOS"), "1_DOS")),
            FieldValue(FieldId("1_DOS"), Text(AnyText, Constant("DOS"), false), "Editable text label", None, None, true, true, true)
          )
        )
      )
    )

    val res = SubmissionService.getSectionFormFields(form, formTemplate)

    res.right.value should be(expectedResult)
  }

  implicit lazy val hc = new HeaderCarrier()
}
