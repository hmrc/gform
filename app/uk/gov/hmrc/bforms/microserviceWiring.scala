/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms

import akka.NotUsed
import akka.stream.IOResult
import akka.stream.scaladsl.{ FileIO, Source }
import akka.util.ByteString
import com.ning.http.multipart.ByteArrayPartSource
import java.io.File
import play.api.libs.json.{ Json, Writes }
import play.api.mvc.MultipartFormData.{ DataPart, FilePart }
import play.api.http.HttpVerbs.{ POST => POST_VERB }
import scala.concurrent.Future
import uk.gov.hmrc.bforms.model.FileId
import uk.gov.hmrc.play.audit.http.config.LoadAuditingConfig
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.auth.microservice.connectors.AuthConnector
import uk.gov.hmrc.play.config.{ AppName, RunMode, ServicesConfig }
import uk.gov.hmrc.play.http.HttpReads
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.ws._
import uk.gov.hmrc.play.audit.http.HttpAuditing

object WSHttp extends WSGet with WSPut with WSPost with WSDelete with HttpAuditing with AppName {
  val auditConnector: AuditConnector = MicroserviceAuditConnector
  override val hooks: Seq[HttpHook] = Seq(AuditingHook)
}

object FusFeUploadWS extends WSPost with HttpAuditing with AppName {

  def doFormPartPost(
    url: String,
    fileName: String,
    contentType: String,
    body: ByteString,
    headers: Seq[(String, String)]
  )(
    implicit
    hc: HeaderCarrier,
    rds: HttpReads[HttpResponse]
  ): Future[HttpResponse] = {
    val source = Source(FilePart(fileName, fileName, Some(contentType), Source.single(body)) :: Nil)
    withTracing(POST_VERB, url) {
      val httpResponse = buildRequest(url).withHeaders(headers: _*).post(source).map(new WSHttpResponse(_))
      //executeHooks(url, POST_VERB, Option(Json.stringify(wts.writes(body))), httpResponse)
      mapErrors(POST_VERB, url, httpResponse).map(rds.read(POST_VERB, url, _))
    }
  }
  val auditConnector: AuditConnector = MicroserviceAuditConnector
  override val hooks: Seq[HttpHook] = Seq.empty[HttpHook]
}

object MicroserviceAuditConnector extends AuditConnector {
  override lazy val auditingConfig = LoadAuditingConfig(s"auditing")
}

object MicroserviceAuthConnector extends AuthConnector with ServicesConfig {
  override val authBaseUrl = baseUrl("auth")
}
