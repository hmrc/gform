/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sdes

import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CorrelationId, SdesSubmission }
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ ExecutionContext, Future }

class SdesModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  mongoModule: MongoModule,
  objectStoreModule: ObjectStoreModule
)(implicit ex: ExecutionContext) {

  private val sdesBaseUrl = configModule.serviceConfig.baseUrl("sdes")
  private val sdesBasePath = configModule.serviceConfig.getString("microservice.services.sdes.base-path")
  private val sdesAuthorizationToken = configModule.serviceConfig.getString("microservice.services.sdes.api-key")
  private val sdesInformationType = configModule.serviceConfig.getString("microservice.services.sdes.information-type")
  private val sdesRecipientOrSender =
    configModule.serviceConfig.getString("microservice.services.sdes.recipient-or-sender")
  private val fileLocationUrl = configModule.serviceConfig.getString("microservice.services.sdes.file-location-url")

  private val sdesHeaders: Seq[(String, String)] = Seq(
    "x-client-id"  -> sdesAuthorizationToken,
    "Content-Type" -> "application/json"
  )

  private val repoSdesSubmission: Repo[SdesSubmission] =
    new Repo[SdesSubmission](
      "sdesSubmission",
      mongoModule.mongoComponent,
      _._id.value,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("confirmedAt"),
          IndexOptions()
            .background(false)
            .name("confirmedAtIndex")
            .expireAfter(configModule.appConfig.`sdes-confirmation-ttl-days`.days.toMillis, TimeUnit.MILLISECONDS)
        )
      )
    )

  val sdesConnector: SdesConnector =
    new SdesConnector(wSHttpModule.auditableWSHttp, sdesBaseUrl, sdesBasePath, sdesHeaders)

  val sdesService: SdesAlgebra[Future] =
    new SdesService(sdesConnector, repoSdesSubmission, sdesRecipientOrSender, sdesInformationType, fileLocationUrl)

  val sdesCallbackController: SdesCallbackController =
    new SdesCallbackController(configModule.controllerComponents, sdesService, objectStoreModule.objectStoreService)

  val foptSdesService: SdesAlgebra[FOpt] = new SdesAlgebra[FOpt] {
    override def notifySDES(envelopeId: EnvelopeId, objWithSummary: ObjectSummaryWithMd5)(implicit
      hc: HeaderCarrier
    ): FOpt[Unit] =
      fromFutureA(sdesService.notifySDES(envelopeId, objWithSummary))

    override def saveSdesSubmission(sdesSubmission: SdesSubmission): FOpt[Unit] =
      fromFutureA(sdesService.saveSdesSubmission(sdesSubmission))

    override def findSdesSubmission(correlationId: CorrelationId): FOpt[Option[SdesSubmission]] =
      fromFutureA(sdesService.findSdesSubmission(correlationId))
  }

}