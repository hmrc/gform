/*
 * Copyright 2026 HM Revenue & Customs
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

import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.scheduler.TraceableWorkItem
import uk.gov.hmrc.gform.scheduler.asynchandlebars.AsyncHandlebarsWorkItem
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HttpMethod, ProfileName }
import uk.gov.hmrc.gform.submission.{ WorkItemHistory, WorkItemHistoryAlgebra }
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.client.RequestBuilder
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

trait AsyncHandlebarsApiExecutor[F[_]] {
  def callAPI(workItem: TraceableWorkItem[AsyncHandlebarsWorkItem])(implicit hc: HeaderCarrier): F[Unit]
}

class RealAsyncHandlebarsApiExecutor(
  buildRequest: (ProfileName, EnvelopeId, String, HttpMethod, HeaderCarrier) => RequestBuilder,
  workItemHistoryService: WorkItemHistoryAlgebra[Future]
)(implicit ec: ExecutionContext)
    extends AsyncHandlebarsApiExecutor[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def callAPI(
    workItem: TraceableWorkItem[AsyncHandlebarsWorkItem]
  )(implicit hc: HeaderCarrier): Future[Unit] = {
    val requestBuilder = buildRequest(
      workItem.data.profile,
      workItem.envelopeId,
      workItem.data.uri,
      workItem.data.method,
      hc
    )

    def send(body: String): Future[HttpResponse] =
      requestBuilder
        .setHeader("Content-Type" -> workItem.data.contentType.value)
        .withBody(body)
        .execute[HttpResponse]

    for {
      response <- workItem.data.method match {
                    case HttpMethod.GET                   => requestBuilder.execute[HttpResponse]
                    case HttpMethod.POST | HttpMethod.PUT => send(workItem.data.payload)
                  }
      history = WorkItemHistory.create(
                  workItem.envelopeId,
                  workItem.formTemplateId,
                  workItem.submissionRef,
                  workItem.destinationId,
                  response.status,
                  response.body
                )
      _ <- workItemHistoryService.save(history)
    } yield
      if (response.status >= 200 && response.status < 300) {
        logger.info(
          s"Successfully called async API for form template ${workItem.formTemplateId.value}, destination id ${workItem.destinationId.id}. Response status: ${response.status}"
        )
      } else {
        throw new RuntimeException(
          s"Failed to call async API for form template ${workItem.formTemplateId.value}, destination id ${workItem.destinationId.id}. Response status: ${response.status}, body: ${response.body}"
        )
      }
  }
}
