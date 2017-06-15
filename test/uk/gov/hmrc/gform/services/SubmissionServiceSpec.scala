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
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses._
import uk.gov.hmrc.play.http.HttpResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class SubmissionServiceSpec extends Spec with TypeclassFixtures {

  implicit override val patienceConfig = PatienceConfig(timeout = scaled(Span(15000, Millis)), interval = scaled(Span(300, Millis)))

  val form = Form(FormId("form-id"), FormData(FormTypeId("form-type-id"), "1.0.0", "UTF-8", List(FormField(FieldId("firstName"), "Joe"), FormField(FieldId("lastName"), "Doe"))))

  val plainFormTemplate = FormTemplate(Some("schemaId"), FormTypeId("IPT100"), "Insurance Premium Tax Return", "version", "description", "characterSet", DmsSubmission("nino", "BT-NRU-Environmental", "FinanceOpsCorpT"), "submitSuccessUrl", "submitErrorUrl", List.empty[Section])

  val yourDetailsSection = Section(
    "Your details",
    None, None,
    List(
      FieldValue(FieldId("firstName"), Text(Constant(""), total = false), "Your first name", None, mandatory = true, editable = true, submissible = true),
      FieldValue(FieldId("lastName"), Text(Constant(""), total = false), "Your last name", None, mandatory = true, editable = true, submissible = true)
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

    implicit val postCreateEnvelope = PostTC
      .response[CreateEnvelope, HttpResponse](
        HttpResponse(
          responseStatus = 200,
          responseHeaders = Map(LOCATION -> List("envelopes/123"))
        )
      )
      .callCheck(postCheck)
      .noChecks

    implicit val insertSubmission: Insert[Submission] = InsertTC
      .response(Right(UpdateSuccess))
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
    (postCheck.call _).expects().repeat(4)

    val res = SubmissionService.submission(FormTypeId("form-type-id"), FormId("form-id"))

    futureResult(res.value).right.value should be("http://localhost:8898/file-transfer/envelopes/123")
  }
}
