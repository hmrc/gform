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

package uk.gov.hmrc.gform.submission.handlebars

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HttpMethod, ProfileName }
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.{ HeaderCarrier, StringContextOps }
import uk.gov.hmrc.http.client.RequestBuilder

import java.time.format.DateTimeFormatter
import java.time.{ ZoneOffset, ZonedDateTime }
import scala.concurrent.ExecutionContext
import scala.util.matching.Regex

class HandlebarsHttpApiModule(
  wSHttpModule: WSHttpModule,
  configModule: ConfigModule
)(implicit ec: ExecutionContext) {

  implicit val hc: HeaderCarrier = new HeaderCarrier()
  private val checkToken: Regex = "^\\{(.*)}$".r
  private val dateFormat: Regex = "^dateFormat\\((.*)\\)$".r

  private def getDynamicHeaderValue(token: String, envelopeId: EnvelopeId): String = token match {
    case "envelopeId"  => envelopeId.value
    case dateFormat(p) => DateTimeFormatter.ofPattern(p).format(ZonedDateTime.now(ZoneOffset.UTC))
    case _             => token
  }

  private def appendUriSegment(base: String, toAppend: String): String =
    if (toAppend.isEmpty) base
    else if (base.endsWith("/") || toAppend.startsWith("/")) s"$base$toAppend"
    else s"$base/$toAppend"

  def buildRequest(
    profile: ProfileName,
    envelopeId: EnvelopeId,
    uri: String,
    method: HttpMethod
  )(implicit hc: HeaderCarrier): RequestBuilder = {
    val profileConfig = configModule.DestinationsServicesConfig()(profile)
    val fullUrl = appendUriSegment(profileConfig.baseUrl, uri)

    val headers: Seq[(String, String)] = hc.extraHeaders ++ profileConfig.httpHeaders.map {
      case (k, checkToken(v)) => k -> getDynamicHeaderValue(v, envelopeId)
      case (k, v)             => k -> v
    }.toSeq ++
      profileConfig.authorization
        .orElse(hc.authorization)
        .map(auth => "Authorization" -> auth.value)

    val builder = method match {
      case HttpMethod.GET  => wSHttpModule.httpClient.get(url"$fullUrl")
      case HttpMethod.POST => wSHttpModule.httpClient.post(url"$fullUrl")
      case HttpMethod.PUT  => wSHttpModule.httpClient.put(url"$fullUrl")
    }

    builder.setHeader(headers: _*)
  }

  val handlebarsHttpSubmitter: HandlebarsHttpApiSubmitter =
    new RealHandlebarsHttpApiSubmitter(buildRequest)

}
