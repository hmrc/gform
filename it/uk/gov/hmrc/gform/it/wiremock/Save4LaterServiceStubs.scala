package uk.gov.hmrc.gform.it.wiremock

import java.time.LocalDateTime

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.{ ok, stubFor }
import play.api.libs.json.Json
import uk.gov.hmrc.crypto.PlainText
import uk.gov.hmrc.gform.it.ITSpec
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

trait Save4LaterServiceStubs {
  this: ITSpec =>
  def save4laterPUTStub(userId: String, formTemplateId: String) = {
    stubFor(
      WireMock
        .put(s"/save4later/gform/$userId-$formTemplateId/data/form")
        .willReturn(
          ok()
            .withBody(s"""{
                         |    "id": "$userId-$formTemplateId",
                         |    "data": {
                         |       "form": "{{jsonPath request.body '$$'}}"
                         |    },
                         |    "modifiedDetails": {
                         |        "createdAt": {
                         |            "$$date": 1607527430069
                         |        },
                         |        "lastUpdated": {
                         |            "$$date": 1607527430069
                         |        }
                         |    },
                         |    "atomicId": {
                         |        "$$oid": "5fd0ec06170000164c12ecaa"
                         |    }
                         |}""".stripMargin)
            .withTransformers("response-template")
        )
    )
    ()
  }

  def save4laterGETStub(userId: String, formTemplateId: String, formData: FormData, status: FormStatus) = {
    val form = Form(
      FormId("123-BASIC"),
      EnvelopeId("some-envelope-id"),
      UserId("123"),
      FormTemplateId("BASIC"),
      formData,
      status,
      VisitIndex.empty,
      ThirdPartyData.empty,
      Some(EnvelopeExpiryDate(LocalDateTime.now().plusDays(28))),
      FormComponentIdToFileIdMapping.empty
    )
    stubFor(
      WireMock
        .get(s"/save4later/gform/$userId-$formTemplateId")
        .willReturn(
          ok()
            .withBody(s"""{
                         |    "id": "$userId-$formTemplateId",
                         |    "data": {
                         |       "form": "${jsonCrypto.encrypt(PlainText(Json.toJson(form).toString)).value}"
                         |    },
                         |    "modifiedDetails": {
                         |        "createdAt": {
                         |            "$$date": 1607527430069
                         |        },
                         |        "lastUpdated": {
                         |            "$$date": 1607527430069
                         |        }
                         |    },
                         |    "atomicId": {
                         |        "$$oid": "5fd0ec06170000164c12ecaa"
                         |    }
                         |}""".stripMargin)
        )
    )
    ()
  }
}
