/*
 * Copyright 2020 HM Revenue & Customs
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

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ MustMatchers, WordSpec }
import play.api.libs.json.{ Reads, _ }
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FormTemplatesControllerRequestHandlerTest extends WordSpec with MustMatchers with ScalaFutures { // TODO Change to use an Id monad

  implicit val defaultPatience = PatienceConfig(timeout = Span(6, Seconds), interval = Span(5, Millis))

  "handle a valid upsert request with Destinations section" in {
    withFixture(
      Json.parse(
        validRequestBodyWithDestinations("hmrc", "${user.enrolledIdentifier}", Some(""""serviceId": "someId",""")))) {
      (sideEffect, verifySideEffect, templateRaw) =>
        val handler = new FormTemplatesControllerRequestHandler(_ => verifySideEffect.get, _ => sideEffect)
        val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

        whenReady(eventualResult.value) { response =>
          response mustBe Right(())
        }
    }
  }

  "handle a valid upsert request with Print section" in {
    withFixture(
      Json.parse(
        validRequestBodyWithPrintSection("hmrc", "${user.enrolledIdentifier}", Some(""""serviceId": "someId",""")))) {
      (sideEffect, verifySideEffect, templateRaw) =>
        val handler = new FormTemplatesControllerRequestHandler(_ => verifySideEffect.get, _ => sideEffect)
        val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

        whenReady(eventualResult.value) { response =>
          response mustBe Right(())
        }
    }
  }

  "handle an invalid upsert request with no Destinations or Print section" in {
    withFixture(
      Json.parse(
        invalidRequestBodyWithNoDestinationsOrPrintSection(
          "hmrc",
          "${user.enrolledIdentifier}",
          Some(""""serviceId": "someId",""")))) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response must matchPattern {
          case Left(UnexpectedState(_)) =>
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
          Some(""""serviceId": "someId",""")))) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response must matchPattern {
          case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  "handle an invalid identifier upsert request" in {
    withFixture(
      Json.parse(validRequestBodyWithPrintSection("hmrc", "${user.broken}", Some(""""serviceId": "someId",""")))) {
      (sideEffect, _, templateRaw) =>
        val handler = new FormTemplatesControllerRequestHandler(_ => sideEffect, _ => sideEffect)
        val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

        whenReady(eventualResult.value) { response =>
          response must matchPattern {
            case Left(UnexpectedState(_)) =>
          }
        }
    }
  }

  "return an error when identifier is ${user.enrolledIdentifier} && authConf is HmrcSimpleModule or HmrcAgentModule" in {
    withFixture(Json.parse(validRequestBodyWithPrintSection("hmrc", "${user.enrolledIdentifier}", None))) {
      (sideEffect, verifySideEffect, templateRaw) =>
        val handler = new FormTemplatesControllerRequestHandler(_ => verifySideEffect.get, _ => sideEffect)
        val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

        whenReady(eventualResult.value) { response =>
          response must matchPattern {
            case Left(UnexpectedState(_)) =>
          }
        }
    }
  }

  private def withFixture(json: JsValue)(f: (FOpt[Unit], Option[FOpt[Unit]], FormTemplateRaw) => Any) = {

    val sideEffect: FOpt[Unit] = fromFutureA(Future.successful(()))
    val templateRaw = implicitly[Reads[FormTemplateRaw]].reads(json).get
    val formTemplate: Option[FormTemplate] = FormTemplate.transformAndReads(json).asOpt
    val verifySideEffect: Option[FOpt[Unit]] = formTemplate.map(formTemplate => new Verifier {}.verify(formTemplate))

    f(sideEffect, verifySideEffect, templateRaw)
  }

  private def validRequestBodyWithDestinations(
    authModule: String,
    identifier: String,
    serviceId: Option[String] = Some(""""serviceId": "Id",""")) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "destinations": [
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
       |      "format": "text",
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

  private def validRequestBodyWithPrintSection(
    authModule: String,
    identifier: String,
    serviceId: Option[String] = Some(""""serviceId": "Id",""")) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "printSection": "TestPrintSection",
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
       |      "format": "text",
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

  private def invalidRequestBodyWithNoDestinationsOrPrintSection(
    authModule: String,
    identifier: String,
    serviceId: Option[String] = Some(""""serviceId": "Id",""")) =
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
       |      "format": "text",
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
    serviceId: Option[String] = Some(""""serviceId": "Id",""")) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "printSection": "TestPrintSection",
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
       |      "format": "text",
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

}
