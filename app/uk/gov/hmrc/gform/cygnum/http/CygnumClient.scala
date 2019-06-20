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

package uk.gov.hmrc.gform.cygnum.http

import cats.Monad
import com.softwaremill.sttp.quick.{ emptyRequest, _ }
import com.softwaremill.sttp.{ Id, RequestT, Response, StatusCode }
import uk.gov.hmrc.gform.config.CygnumConfig

class CygnumClient[F[_]] extends CygnumConfig {

  def sendRequest(payload: String)(implicit M: Monad[F]): F[CygnumResponse] = {
    val urnReq: RequestT[Id, String, Nothing] = emptyRequest
      .post(uri"$cygnumURL")
      .body(payload)
      .header("Content-Type", "application/soap+xml; charset=utf-8", true)

    val response: Id[Response[String]] = urnReq.send()

    M.pure(CygnumResponse(response.code, response.body))
  }
}

case class CygnumResponse(status: StatusCode, body: Either[String, String])
