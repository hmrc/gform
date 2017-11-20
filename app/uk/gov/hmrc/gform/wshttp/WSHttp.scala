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

package uk.gov.hmrc.gform.wshttp

import java.nio.file.Path

import akka.NotUsed
import akka.stream.IOResult
import akka.stream.scaladsl.FileIO
import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.http.HttpVerbs.{ POST => POST_VERB }
import play.api.mvc.MultipartFormData.FilePart
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.play.http.ws.WSHttpResponse

import scala.concurrent.Future
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads }
import uk.gov.hmrc.http.hooks.HttpHook

class WSHttp(httpHooks: Seq[HttpHook] = Nil) extends uk.gov.hmrc.play.http.ws.WSHttp {

  override val hooks: Seq[HttpHook] = httpHooks

  //TODO: body should be type of Stream not ByteString (do we want to blow up if few people will submit forms at the same time?)
  def POSTFile[O](
    url: String,
    fileName: String,
    body: ByteString,
    headers: Seq[(String, String)],
    contentType: String //TODO: change type to ContentType
  )(implicit
    hc: HeaderCarrier,
    rds: HttpReads[O]): Future[O] = {

    val source = Source(FilePart(fileName, fileName, Some(contentType), Source.single(body)) :: Nil)
    withTracing(POST_VERB, url) {
      import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
      val httpResponse = buildRequest(url).withHeaders(headers: _*).post(source).map(new WSHttpResponse(_))
      executeHooks(url, POST_VERB, Option(s"""{"info":"multipart upload of $fileName"}"""), httpResponse)
      mapErrors(POST_VERB, url, httpResponse).map(rds.read(POST_VERB, url, _))
    }
  }

}
