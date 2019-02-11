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

package uk.gov.hmrc.gform.wshttp

import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.mvc.MultipartFormData.FilePart
import uk.gov.hmrc.play.http.ws._

import scala.concurrent.Future
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.hooks.HttpHooks
import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.auth.microservice.connectors.AuthConnector
import uk.gov.hmrc.play.config.{ AppName, ServicesConfig }
import uk.gov.hmrc.play.microservice.config.LoadAuditingConfig

object MicroserviceAuditConnector extends AuditConnector {
  lazy val auditingConfig: AuditingConfig = LoadAuditingConfig(s"auditing")
}

trait Hooks extends HttpHooks with HttpAuditing {
  override val hooks = Seq(AuditingHook)
  override lazy val auditConnector: AuditConnector = MicroserviceAuditConnector
}

trait WSHttp
    extends HttpGet with WSGet with HttpPut with WSPut with HttpPost with WSPost with HttpDelete with WSDelete
    with Hooks with AppName {

  //TODO: body should be type of Stream not ByteString (do we want to blow up if few people will submit forms at the same time?)
  def POSTFile[O](
    url: String,
    fileName: String,
    body: ByteString,
    headers: Seq[(String, String)],
    contentType: String //TODO: change type to ContentType
  )(
    implicit
    hc: HeaderCarrier,
    rds: HttpReads[O]): Future[HttpResponse] = {

    val source: Source[FilePart[Source[ByteString, NotUsed]], NotUsed] = Source(
      FilePart(fileName, fileName, Some(contentType), Source.single(body)) :: Nil)
    //    withTracing(POST_VERB, url) {
    //      import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
    //      val httpResponse = buildRequest(url).withHeaders(headers: _*).post(source).map(new WSHttpResponse(_))
    //      executeHooks(url, POST_VERB, Option(s"""{"info":"multipart upload of $fileName"}"""), httpResponse)
    //      mapErrors(POST_VERB, url, httpResponse).map(rds.read(POST_VERB, url, _))
    //    }
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    buildRequest(url).withHeaders(headers: _*).post(source).map(new WSHttpResponse(_))
  }

}

object WSHttp extends WSHttp

object MicroserviceAuthConnector extends AuthConnector with ServicesConfig with WSHttp {
  override val authBaseUrl: String = baseUrl("auth")
}

//class WSHttp(httpHooks: Seq[HttpHook] = Nil) extends uk.gov.hmrc.play.http.ws.WSHttp {
//
//  override val hooks: Seq[HttpHook] = httpHooks
//
//  //TODO: body should be type of Stream not ByteString (do we want to blow up if few people will submit forms at the same time?)
//  def POSTFile[O](
//    url: String,
//    fileName: String,
//    body: ByteString,
//    headers: Seq[(String, String)],
//    contentType: String //TODO: change type to ContentType
//  )(implicit
//    hc: HeaderCarrier,
//    rds: HttpReads[O]): Future[HttpResponse] = {
//
//    val source: Source[FilePart[Source[ByteString, NotUsed]], NotUsed] = Source(FilePart(fileName, fileName, Some(contentType), Source.single(body)) :: Nil)
////    withTracing(POST_VERB, url) {
////      import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
////      val httpResponse = buildRequest(url).withHeaders(headers: _*).post(source).map(new WSHttpResponse(_))
////      executeHooks(url, POST_VERB, Option(s"""{"info":"multipart upload of $fileName"}"""), httpResponse)
////      mapErrors(POST_VERB, url, httpResponse).map(rds.read(POST_VERB, url, _))
////    }
//    import play.api.libs.concurrent.Execution.Implicits.defaultContext
//
//    buildRequest(url).withHeaders(headers: _*).post(source).map(new WSHttpResponse(_))
//  }
//}
