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

package uk.gov.hmrc.gform.handlebarspayload

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsPayload, HandlebarsPayloadId }

import scala.concurrent.{ ExecutionContext, Future }

class HandlebarsPayloadModule(controllerComponents: ControllerComponents, mongoModule: MongoModule)(implicit
  ex: ExecutionContext
) {

  private val handlebarsPayloadRepo: Repo[HandlebarsPayload] =
    new Repo[HandlebarsPayload]("handlebarsPayload", mongoModule.mongoComponent, _._id.value)

  val handlebarsPayloadService: HandlebarsPayloadAlgebra[Future] = new HandlebarsPayloadService(
    handlebarsPayloadRepo
  )

  val handlebarPayloadController: HandlebarsPayloadController =
    new HandlebarsPayloadController(controllerComponents, handlebarsPayloadService)

  val foptHandlebarsPayloadService: HandlebarsPayloadAlgebra[FOpt] = new HandlebarsPayloadAlgebra[FOpt] {
    override def save(handlebarsPayload: HandlebarsPayload): FOpt[Unit] =
      fromFutureA(handlebarsPayloadService.save(handlebarsPayload))

    override def get(handlebarsPayloadName: HandlebarsPayloadId): FOpt[HandlebarsPayload] =
      fromFutureA(handlebarsPayloadService.get(handlebarsPayloadName))
  }

}
