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

  "handle a valid upsert request" in {
    withFixture("${user.enrolledIdentifier}") { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response mustBe Right(())
      }
    }
  }

  "handle an invalid identifier upsert request" in {
    withFixture("${user.broken}") { (sideEffect, _, templateRaw) =>
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
    withFixture("${user.enrolledIdentifier}", None) { (sideEffect, verifySideEffect, templateRaw) =>
      val handler = new FormTemplatesControllerRequestHandler(_ => verifySideEffect.get, _ => sideEffect)
      val eventualResult = handler.futureInterpreter.handleRequest(templateRaw)

      whenReady(eventualResult.value) { response =>
        response must matchPattern {
          case Left(UnexpectedState(_)) =>
        }
      }
    }
  }

  private def withFixture(identifier: String, serviceId: Option[String] = Some(""""serviceId": "someId","""))(
    f: (FOpt[Unit], Option[FOpt[Unit]], FormTemplateRaw) => Any) = {

    val sideEffect: FOpt[Unit] = fromFutureA(Future.successful(()))
    val json: JsValue = Json.parse(requestBody("hmrc", identifier, serviceId))
    val templateRaw = implicitly[Reads[FormTemplateRaw]].reads(json).get
    val formTemplate: Option[FormTemplate] = FormTemplate.transformAndReads(json).asOpt
    val verifySideEffect: Option[FOpt[Unit]] = formTemplate.map(formTemplate => new Verifier {}.verify(formTemplate))

    f(sideEffect, verifySideEffect, templateRaw)
  }

  private def requestBody(
    authModule: String,
    identifier: String,
    serviceId: Option[String] = Some(""""serviceId": "Id",""")) =
    s"""{
       |  "_id": "newfield",
       |  "formName": "Testing section change label tttt",
       |  "description": "Testing the form change label",
       |  "languages":["en"],
       |  "dmsSubmission": {
       |    "dmsFormId": "",
       |    "customerId": "$${auth.payenino}",
       |    "classificationType": "",
       |    "businessArea": ""
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
