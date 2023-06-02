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

package uk.gov.hmrc.gform.handlebarstemplate

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.{ DeleteResult, Repo }
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsTemplate, HandlebarsTemplateId }

import scala.concurrent.{ ExecutionContext, Future }

class HandlebarsTemplateModule(controllerComponents: ControllerComponents, mongoModule: MongoModule)(implicit
  ex: ExecutionContext
) {

  private val handlebarsTemplateRepo: Repo[HandlebarsTemplate] =
    new Repo[HandlebarsTemplate]("handlebarsTemplate", mongoModule.mongoComponent, _._id.value)

  private val handlebarsTemplateService: HandlebarsTemplateAlgebra[Future] = new HandlebarsTemplateService(
    handlebarsTemplateRepo
  )

  val handlebarTemplateController: HandlebarsTemplateController =
    new HandlebarsTemplateController(controllerComponents, handlebarsTemplateService)

  val foptHandlebarsPayloadService: HandlebarsTemplateAlgebra[FOpt] = new HandlebarsTemplateAlgebra[FOpt] {
    override def save(handlebarsTemplate: HandlebarsTemplate): FOpt[Unit] =
      fromFutureA(handlebarsTemplateService.save(handlebarsTemplate))

    override def get(handlebarsTemplateId: HandlebarsTemplateId): FOpt[Option[HandlebarsTemplate]] =
      fromFutureA(handlebarsTemplateService.get(handlebarsTemplateId))

    override def delete(handlebarsTemplateId: HandlebarsTemplateId): FOpt[DeleteResult] =
      fromFutureA(handlebarsTemplateService.delete(handlebarsTemplateId))

    override def getAll: FOpt[List[HandlebarsTemplateId]] =
      fromFutureA(handlebarsTemplateService.getAll)
  }

}
