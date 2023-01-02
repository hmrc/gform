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

package uk.gov.hmrc.gform

import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.{ ExecutionContext, Future }

package object wshttp {
  implicit class FutureHttpResponseSyntax(fr: Future[HttpResponse]) {
    def failWithNonSuccessStatusCodes(url: String)(implicit ec: ExecutionContext): Future[HttpResponse] =
      fr.flatMap { response =>
        val status = response.status
        if (status >= 200 && status < 300) {
          Future.successful(response)
        } else {
          Future.failed(new Exception(s"POST to $url failed with status $status. Response body: '${response.body}'"))
        }
      }
  }

  implicit class HttpResponseSyntax(response: HttpResponse) {
    def isSuccess: Boolean = response.status >= 200 && response.status < 300
  }
}
