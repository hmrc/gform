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

package uk.gov.hmrc.gform

import com.softwaremill.sttp._
import org.scalatest.concurrent.Eventually
import org.scalatest.{BeforeAndAfterEach, MustMatchers, WordSpec}
import org.scalatest.time.{Millis, Seconds, Span}
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{FormTemplate, FormTemplateId}
import scala.concurrent.ExecutionContext.Implicits.global

class OfstedReviewSubmitterITSpec extends WordSpec with Eventually with MustMatchers with BeforeAndAfterEach with StubServer {

  implicit val backend = HttpURLConnectionBackend()
  implicit override val patienceConfig =
    PatienceConfig(timeout = scaled(Span(20, Seconds)), interval = scaled(Span(5, Millis)))

  "create and persist an ofsted review form" in {
    val templateId = "999"
    persistAFormTemplate(FormTemplateId(templateId))

    eventually {
      val response = sttp.get(Uri("localhost", port, List("gform", "formtemplates", templateId))).send()

      response.code mustBe StatusCodes.Ok
      Json.parse(response.body.right.get).as[FormTemplate]._id mustBe FormTemplateId(templateId)
    }
  }

  def persistAFormTemplate(formTemplateId: FormTemplateId) =
    stubbedModule.module.formTemplateModule.formTemplateService.verifyAndSave(Json.parse(js).as[FormTemplate])

  override def beforeEach(): Unit =
    stubbedModule.module.formTemplateModule.formTemplateService.list map {
      case ids: List[String] =>
        ids.foreach(id => stubbedModule.module.formTemplateModule.formTemplateService.delete(FormTemplateId(id)))
    }

  val js =
    """
      |{
      |  "_id": "999",
      |  "formName": "Register for Aggregates Levy",
      |  "description": "",
      |  "developmentPhase": "beta",
      |  "emailTemplateId": "al_registration_confirmation",
      |  "authConfig": {
      |    "authModule": "hmrc"
      |  },
      |  "sections": [
      |    {
      |      "title": "title",
      |      "shortName": "shortname",
      |      "fields": [
      |        {
      |          "id": "nameOfSchool",
      |          "type": "text",
      |          "label": "Name of School",
      |          "errorMessage": ""
      |        }
      |      ]
      |    }
      |  ],
      |  "declarationSection": {
      |    "title": "Confirm details and send your registration",
      |    "fields": [
      |      {
      |        "id": "declarationFullName",
      |        "type": "text",
      |        "label": "Full name",
      |        "errorMessage": "Enter full name"
      |      }
      |    ]
      |  },
      |  "acknowledgementSection": {
      |    "title": "Your registration has been submitted",
      |    "fields": [
      |      {
      |        "id": "ackPageInfoTop",
      |        "type": "info",
      |        "label": "",
      |        "infoText": "This is your last opportunity to print or save a PDF copy of your submitted registration.",
      |        "infoType": "noformat"
      |      },
      |      {
      |        "id": "ackPageInfoBottom",
      |        "type": "info",
      |        "label": "",
      |        "infoText": "You must check the guidance for this service on GOV.UK to see whether you need to fill in and send any additional forms to complete your registration (for example if your business is a partnership you must also fill in and send form AL2) if you haven't already done so.\n\nHMRC will send you a letter to confirm your registration details and return dates so that you can prepare your first Aggregates Levy return. \n\nReturns will normally cover a 3 month period but if you would like to apply for non-standard periods, such as monthly returns, or periods that match your VAT return periods, you can contact the HMRC Excise team on enquiries.eeitts@hmrc.gsi.gov.uk\n\nIf you do not use a weighbridge you will also need to get in touch with the Excise team to agree an alternative method for weighing your aggregates.",
      |        "infoType": "noformat"
      |      }
      |    ]
      |  },
      |  "destinations": [
      |    {
      |      "id": "createReview",
      |      "type": "reviewingOfsted",
      |      "correlationFieldId": "idOfFormBeingReviewed",
      |      "reviewFormTemplateId": "reviewFormTemplate",
      |      "userId": "ofstedUsers"
      |    },
      |    {
      |      "id": "unattendedDmsQueueDestination",
      |      "type": "hmrcDms",
      |      "includeIf": "true",
      |      "failOnError": true,
      |      "convertSingleQuotes": true,
      |      "dmsFormId": "ALREG",
      |      "customerId": "${declarationFullName}",
      |      "classificationType": "BT-CTO-EMCS Fallback",
      |      "businessArea": "BT"
      |    }
      |  ],
      |  "submitErrorUrl": "",
      |  "submitSuccessUrl": ""
      |}
    """.stripMargin

}
