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

package uk.gov.hmrc.gform.formtemplate

import org.scalatest.{ Matchers, WordSpec }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.json._
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FormTemplatesControllerRequestHandlerTest extends WordSpec with Matchers with ScalaFutures { // TODO Change to use an Id monad

  implicit val defaultPatience = PatienceConfig(timeout = Span(6, Seconds), interval = Span(5, Millis))

  "handle a valid upsert request with Destinations section" in {
    withFixture(
      Json.parse(
        validRequestBodyWithDestinations("hmrc", "${user.enrolledIdentifier}", Some(""""serviceId": "someId","""))
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(
        _ => _ => _ => verifySideEffect.get,
        _ => sideEffect
      )
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response shouldBe Right(())
      }
    }
  }

  "handle a valid upsert request with draftRetrievalMethod is notPermitted" in {
    withFixture(
      Json.parse(
        validRequestBodyWithDestinations(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId","""),
          Some(""""draftRetrievalMethod": "notPermitted",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response shouldBe Right(())
      }
    }
  }

  "handle a valid upsert request with Print section" in {
    withFixture(
      Json.parse(
        validRequestBodyWithPrintSection("hmrc", "${user.enrolledIdentifier}", Some(""""serviceId": "someId","""))
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response shouldBe Right(())
      }
    }
  }

  "handle a valid upsert request with Print section having no PdfNotification" in {
    withFixture(
      Json.parse(
        validRequestBodyWithPrintSectionAndNoPdfNotification(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response shouldBe Right(())
      }
    }
  }

  "handle a valid upsert request with Print section having empty PdfNotification" in {
    withFixture(
      Json.parse(
        validRequestBodyWithPrintSectionWithEmptyPdfNotification(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response shouldBe Right(())
      }
    }
  }

  "handle a valid upsert request with Print section And Empty Pdf Header Footer" in {
    withFixture(
      Json.parse(
        validRequestBodyWithPrintSectionAndEmptyPdfHeaderFooter(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response shouldBe Right(())
      }
    }
  }

  "handle a valid upsert request with Destinations section having valid DmsFormIds" in {
    withFixture(
      Json.parse(
        requestBodyWithDestinationsWithDmsFormIds(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""dmsFormId": "TST123","""),
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response shouldBe Right(())
      }
    }
  }

  "handle an invalid upsert request with no Destinations or Print section" in {
    withFixture(
      Json.parse(
        invalidRequestBodyWithNoDestinationsOrPrintSection(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid upsert request with both Destinations and Print sections" in {
    withFixture(
      Json.parse(
        invalidRequestBodyWithBothDestinationsAndPrintSections(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid upsert request with Destinations but acknowledgementSection is missing" in {
    withFixture(
      Json.parse(
        invalidRequestBodyWithDestinationsWithoutAcknowledgementSection(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an upsert request with DeclarationSection missing" in {
    withFixture(
      Json.parse(
        validRequestBodyWithDestinationsWithoutDeclarationSection(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Right(()) =>
        }
      }
    }
  }

  "handle an invalid upsert request with PrintSection and acknowledgementSection is also present" in {
    withFixture(
      Json.parse(
        invalidRequestBodyWithPrintSectionAndAcknowledgementSection(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid upsert request with PrintSection and DeclarationSection is also present" in {
    withFixture(
      Json.parse(
        invalidRequestBodyWithPrintSectionAndDeclarationSection(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid identifier upsert request" in {
    withFixture(
      Json.parse(validRequestBodyWithPrintSection("hmrc", "${user.broken}", Some(""""serviceId": "someId",""")))
    ) { (sideEffect, _, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => sideEffect, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "return an error when identifier is ${user.enrolledIdentifier} && authConf is HmrcSimpleModule or HmrcAgentModule" in {
    withFixture(Json.parse(validRequestBodyWithPrintSection("hmrc", "${user.enrolledIdentifier}", None))) {
      (sideEffect, verifySideEffect, templateRaw) =>
        val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
        val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

        whenReady(eventualResult.value) { response =>
          response should matchPattern { case Left(UnexpectedState(_)) =>
          }
        }
    }
  }

  "handle an invalid upsert request with Destinations having dmsFormId with Zero char" in {
    withFixture(
      Json.parse(
        requestBodyWithDestinationsWithDmsFormIds(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""dmsFormId": "","""),
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid upsert request with Destinations having dmsFormId with space" in {
    withFixture(
      Json.parse(
        requestBodyWithDestinationsWithDmsFormIds(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""dmsFormId": " ","""),
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid upsert request with Destinations having dmsFormId with more than 12 chars" in {
    withFixture(
      Json.parse(
        requestBodyWithDestinationsWithDmsFormIds(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""dmsFormId": "TST0123456789","""),
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid upsert request with Destinations missing dmsFormId" in {
    withFixture(
      Json.parse(
        requestBodyWithDestinationsWithDmsFormIds(
          "hmrc",
          "${user.enrolledIdentifier}",
          None,
          Some(""""serviceId": "someId",""")
        )
      )
    ) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => _ => _ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response should matchPattern { case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  private def withFixture(json: JsValue)(f: (FOpt[Unit], Option[FOpt[Unit]], FormTemplateRaw) => Any) = {

    val sideEffect: FOpt[Unit] = fromFutureA(Future.successful(()))
    val templateRaw = implicitly[Reads[FormTemplateRaw]].reads(json).get
    val formTemplate: Option[FormTemplate] = FormTemplate.transformAndReads(json).asOpt
    val verifySideEffect: Option[FOpt[Unit]] =
      formTemplate.map(formTemplate => new Verifier {}.verify(formTemplate))

    f(sideEffect, verifySideEffect, templateRaw)
  }

  private def validRequestBodyWithDestinations(
    authModule: String,
    identifier: String,
    serviceId: Option[String],
    draftRetrievalMethod: Option[String] = None
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  ${draftRetrievalMethod.getOrElse("")}
       |  "languages":["en"],
       |  "destinations": [
       |    {
       |      "id": "HMRCDMS",
       |      "type": "hmrcDms",
       |      "dmsFormId": "TST123",
       |      "customerId": "'123'",
       |      "classificationType": "BT-NRU-Environmental",
       |      "businessArea": "FinanceOpsCorpT"
       |    }
       |  ],
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  },
       |  "acknowledgementSection": {
       |    "shortName": "Acknowledgement Page",
       |    "title": "Acknowledgement Page",
       |    "fields": [
       |      {
       |        "type": "info",
       |        "id": "ackpageInfo",
       |        "label": "SomeContent",
       |        "infoText": "SomeContent"
       |      }
       |    ]
       |  }
       |}""".stripMargin

  private def validRequestBodyWithPrintSection(authModule: String, identifier: String, serviceId: Option[String]) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |    "printSection": {
       |        "page": {
       |            "title": "Test Title",
       |            "instructions": "Test Instructions"
       |        },
       |        "pdf": {
       |        	"header": "##Test PrintPDF Header",
       |        	"footer": "##Test PrintPDF Footer"
       |        },
       |        "pdfNotification": {
       |        	"header": "##Test NotificationPDF Header",
       |        	"footer": "##Test NotificationPDF Footer",
       |        	"fieldIds": ["elementA", "elementB"]
       |        }
       |    },
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }]
       |}""".stripMargin

  private def validRequestBodyWithPrintSectionAndEmptyPdfHeaderFooter(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |    "printSection": {
       |        "page": {
       |            "title": "Test Title",
       |            "instructions": "Test Instructions"
       |        },
       |         "pdf": {
       |        	"header": "",
       |        	"footer": ""
       |        },
       |        "pdfNotification": {
       |        	"header": "##Test PDF Header",
       |        	"footer": "##Test PDF Footer",
       |        	"fieldIds": ["elementA", "elementB"]
       |        }
       |    },
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }]
       |}""".stripMargin

  private def validRequestBodyWithPrintSectionAndNoPdfNotification(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |    "printSection": {
       |        "page": {
       |            "title": "Test Title",
       |            "instructions": "Test Instructions"
       |        },
       |        "pdf": {
       |        	"header": "##Test PrintPDF Header",
       |        	"footer": "##Test PrintPDF Footer"
       |        }
       |    },
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }]
       |}""".stripMargin

  private def validRequestBodyWithPrintSectionWithEmptyPdfNotification(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |    "printSection": {
       |        "page": {
       |            "title": "Test Title",
       |            "instructions": "Test Instructions"
       |        },
       |        "pdf": {
       |        	"header": "##Test PrintPDF Header",
       |        	"footer": "##Test PrintPDF Footer"
       |        },
       |        "pdfNotification": {
       |        	"header": "",
       |        	"footer": "",
       |        	"fieldIds": []
       |        }
       |    },
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }]
       |}""".stripMargin

  private def invalidRequestBodyWithNoDestinationsOrPrintSection(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  },
       |  "acknowledgementSection": {
       |    "title": "",
       |    "fields": []
       |  }
       |}""".stripMargin

  private def invalidRequestBodyWithBothDestinationsAndPrintSections(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "printSection": {
       |    "title": "Next Steps",
       |    "summaryPdf": "TestSummaryPdf"
       |  },
       |   "destinations": [
       |        {
       |            "id": "transitionToSubmitted",
       |            "type": "stateTransition",
       |            "requiredState": "Submitted"
       |        }
       |   ],
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  },
       |  "acknowledgementSection": {
       |    "title": "",
       |    "fields": []
       |  }
       |}""".stripMargin

  private def invalidRequestBodyWithDestinationsWithoutAcknowledgementSection(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |   "destinations": [
       |        {
       |            "id": "transitionToSubmitted",
       |            "type": "stateTransition",
       |            "requiredState": "Submitted"
       |        }
       |   ],
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  }
       |}""".stripMargin

  private def validRequestBodyWithDestinationsWithoutDeclarationSection(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "destinations": [
       |    {
       |      "id": "HMRCDMS",
       |      "type": "hmrcDms",
       |      "dmsFormId": "TST123",
       |      "customerId": "'123'",
       |      "classificationType": "BT-NRU-Environmental",
       |      "businessArea": "FinanceOpsCorpT"
       |    }
       |  ],
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |  "acknowledgementSection": {
       |    "shortName": "Acknowledgement Page",
       |    "title": "Acknowledgement Page",
       |    "fields": [
       |      {
       |        "type": "info",
       |        "id": "ackpageInfo",
       |        "label": "SomeContent",
       |        "infoText": "SomeContent"
       |      }
       |    ]
       |  }
       |}""".stripMargin

  private def invalidRequestBodyWithPrintSectionAndAcknowledgementSection(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "printSection": {
       |    "title": "Next Steps",
       |    "summaryPdf": "TestSummaryPdf"
       |  },
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  },
       |  "acknowledgementSection": {
       |    "title": "",
       |    "fields": []
       |  }
       |}""".stripMargin

  private def invalidRequestBodyWithPrintSectionAndDeclarationSection(
    authModule: String,
    identifier: String,
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "printSection": {
       |    "title": "Next Steps",
       |    "summaryPdf": "TestSummaryPdf"
       |  },
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  }
       |}""".stripMargin

  private def requestBodyWithDestinationsWithDmsFormIds(
    authModule: String,
    identifier: String,
    dmsFormId: Option[String],
    serviceId: Option[String]
  ) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "destinations": [
       |    {
       |      "id": "HMRCDMS1",
       |      "type": "hmrcDms",
       |      ${dmsFormId.getOrElse("")}
       |      "customerId": "'123'",
       |      "classificationType": "BT-NRU-Environmental",
       |      "businessArea": "FinanceOpsCorpT"
       |    },
       |    {
       |      "id": "submitToADJNotify",
       |      "type": "email",
       |      "convertSingleQuotes": true,
       |      "failOnError": false,
       |      "emailTemplateId": "4f438fe6-680d-4610-9e55-b50f711326e4",
       |      "to": "emailAddress",
       |      "personalisation": {
       |        "customerName": "nameHidden"
       |      }
       |    }
       |  ],
       |  "authConfig": {
       |    "authModule": "$authModule",
       |    ${serviceId.getOrElse("")}
       |    "agentAccess": "allowAnyAgentAffinityUser"
       |  },
       |  "emailTemplateId": "",
       |  "sections": [{
       |    "title": "Page A",
       |    "fields": [{
       |      "id": "elementA",
       |      "type": "text",
       |      "format": "sterling",
       |      "value": "$identifier",
       |      "submitMode": "readonly",
       |      "label": "Element A"
       |    },{
       |      "id": "elementB",
       |      "format": "shortText",
       |      "submitMode": "readonly",
       |      "label": "Element B",
       |      "validIf": "$${elementA=''}"
       |    }]
       |  }],
       |
       |  "declarationSection": {
       |    "title": "",
       |    "fields": []
       |  },
       |  "acknowledgementSection": {
       |    "shortName": "Acknowledgement Page",
       |    "title": "Acknowledgement Page",
       |    "fields": [
       |      {
       |        "type": "info",
       |        "id": "ackpageInfo",
       |        "label": "SomeContent",
       |        "infoText": "SomeContent"
       |      }
       |    ]
       |  }
       |}""".stripMargin
}
