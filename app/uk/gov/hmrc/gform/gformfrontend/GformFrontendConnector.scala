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

package uk.gov.hmrc.gform.gformfrontend

import cats.data.EitherT
import cats.implicits.{ catsSyntaxEitherId, catsSyntaxEq }
import play.api.libs.json.{ JsError, JsSuccess }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.repo.DeleteResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ ExecutionContext, Future }

class GformFrontendConnector(httpClient: HttpClientV2, baseUrl: String)(implicit ec: ExecutionContext) {
  implicit val hc: HeaderCarrier = new HeaderCarrier()

  def saveFormTemplateCache(
    formTemplateId: FormTemplateId
  )(implicit ec: ExecutionContext): FOpt[Unit] = EitherT {
    val url = s"$baseUrl/formtemplates/${formTemplateId.value}/cache"
    httpClient
      .post(url"$url")
      .withBody("")
      .execute[HttpResponse]
      .asEither
  }

  def deleteFormTemplateCache(formTemplateId: FormTemplateId)(implicit ec: ExecutionContext): FOpt[DeleteResult] =
    EitherT {
      val url = s"$baseUrl/formtemplates/${formTemplateId.value}/cache"
      httpClient
        .delete(url"$url")
        .execute[HttpResponse]
        .map { httpResponse =>
          val status = httpResponse.status
          if (status === 200) {
            (httpResponse.json.validate[DeleteResult] match {
              case JsSuccess(value, _) => JsSuccess(value)
              case other               => JsError(s"Invalid JSON format for DeleteResult. Json is $other")
            }).fold(
              invalid =>
                UnexpectedState(
                  s"Error parsing response for form template '${formTemplateId.value}' during caching. Parsing error: $invalid. Response body: ${httpResponse.body}."
                ).asLeft,
              valid => valid.asRight
            )
          } else {
            UnexpectedState(
              s"Failed to cache form template '${formTemplateId.value}' with HTTP status $status. Request URL: $url. Response body: ${httpResponse.body}."
            ).asLeft
          }
        }
        .recover { case lastError =>
          UnexpectedState(lastError.getMessage).asLeft
        }
    }

  implicit class FutureWriteResultOps[R](t: Future[R]) {
    def asEither: Future[Either[UnexpectedState, Unit]] =
      t.map { _ =>
        ().asRight[UnexpectedState]
      } recover { case lastError =>
        UnexpectedState(lastError.getMessage).asLeft[Unit]
      }
  }
}
