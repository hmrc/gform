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

package uk.gov.hmrc.gform.commons

import uk.gov.hmrc.http.HttpReads.{ is2xx, is4xx, is5xx }
import uk.gov.hmrc.http._

trait HttpFunctions {
  def jsonHttpReads[A](rds: HttpReads[A]): HttpReads[A] =
    HttpReads.ask.flatMap {
      case (method, url, response) =>
        response.status match {
          case status if is2xx(status) => rds
          case 400                     => throw new BadRequestException(HttpErrorFunctions.badRequestMessage(method, url, response.body))
          case 404                     => throw new NotFoundException(HttpErrorFunctions.notFoundMessage(method, url, response.body))
          case status if is4xx(status) || is5xx(status) =>
            throw UpstreamErrorResponse(
              message = HttpErrorFunctions.upstreamResponseMessage(method, url, status, response.body),
              statusCode = status,
              reportAs = status,
              headers = response.headers
            )
          case other =>
            throw new Exception(s"$method to $url failed with status $other. Response body: '${response.body}'")
        }
    }
}
