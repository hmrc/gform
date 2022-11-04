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

package uk.gov.hmrc.gform.envelope

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import scala.concurrent.{ ExecutionContext, Future }

class EnvelopeModule(mongoModule: MongoModule, configModule: ConfigModule)(implicit
  ex: ExecutionContext
) {
  private val envelopeRepo: Repo[EnvelopeData] =
    new Repo[EnvelopeData]("envelope", mongoModule.mongoComponent, _._id.value)

  val envelopeService: EnvelopeAlgebra[Future] =
    new EnvelopeService(envelopeRepo)

  val envelopeController: EnvelopeController =
    new EnvelopeController(configModule.controllerComponents, envelopeService)

  val foptEnvelopeService: EnvelopeAlgebra[FOpt] = new EnvelopeAlgebra[FOpt] {
    override def save(envelope: EnvelopeData): FOpt[Unit] =
      fromFutureA(envelopeService.save(envelope))

    override def get(envelopeId: EnvelopeId): FOpt[EnvelopeData] =
      fromFutureA(envelopeService.get(envelopeId))
  }
}
