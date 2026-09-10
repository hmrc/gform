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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._
import uk.gov.hmrc.gform.config.AuthorizationName

class DestinationPersistedFormatSpec extends AnyFlatSpec with Matchers {

  private def prune(json: JsValue, key: String): JsValue = json match {
    case JsObject(fields) => JsObject(fields.collect { case (k, v) if k != key => k -> prune(v, key) })
    case JsArray(values)  => JsArray(values.map(prune(_, key)))
    case other            => other
  }

  private val handlebarsHttpApi: Destination = Destination.HandlebarsHttpApi(
    DestinationId("hbs"),
    ProfileName("profile"),
    "/uri",
    HttpMethod.POST,
    Some("{}"),
    TemplateType.JSON,
    DestinationIncludeIf.HandlebarValue("true"),
    failOnError = true,
    multiRequestPayload = false,
    convertSingleQuotes = None,
    credential = Some(AuthorizationName("credential")),
    httpHeaders = Map.empty,
    validateHandlebarPayload = false,
    jsonSchemaName = None,
    jsonSchema = None
  )

  private val asyncHandlebarsHttpApi: Destination = Destination.AsyncHandlebarsHttpApi(
    DestinationId("async-hbs"),
    ProfileName("profile"),
    "/uri",
    HttpMethod.POST,
    Some("{}"),
    TemplateType.JSON,
    DestinationIncludeIf.HandlebarValue("true"),
    failOnError = true,
    convertSingleQuotes = None,
    credential = Some(AuthorizationName("credential")),
    httpHeaders = Map.empty,
    validateHandlebarPayload = false,
    jsonSchemaName = None,
    jsonSchema = None
  )

  "Destination.format" should "read documents persisted before validateHandlebarPayload existed" in {
    List(handlebarsHttpApi, asyncHandlebarsHttpApi).foreach { destination =>
      val persistedBeforeUpgrade = prune(Json.toJson(destination), "validateHandlebarPayload")

      Destination.format.reads(persistedBeforeUpgrade).asOpt shouldBe Some(destination)
    }
  }

  it should "preserve validateHandlebarPayload when it is present" in {
    val enabled = handlebarsHttpApi.asInstanceOf[Destination.HandlebarsHttpApi].copy(validateHandlebarPayload = true)

    Destination.format.reads(Json.toJson(enabled: Destination)).asOpt shouldBe Some(enabled)
  }
}
