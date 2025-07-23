/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.log

import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.Repo

import java.util.concurrent.TimeUnit
import scala.concurrent.{ ExecutionContext, Future }

class DataAccessLogModule(
  controllerComponents: ControllerComponents,
  mongoModule: MongoModule,
  configModule: ConfigModule
)(implicit
  ex: ExecutionContext
) {

  private val logRepo: Repo[DataAccessLog] =
    new Repo[DataAccessLog](
      "dataAccessLog",
      mongoModule.mongoComponent,
      _._id.toString,
      Seq(
        IndexModel(
          Indexes.ascending("createdAt"),
          IndexOptions()
            .background(false)
            .name("createdAtIndex")
            .expireAfter(configModule.appConfig.`sensitive-data-access-log-ttl`.toMillis, TimeUnit.MILLISECONDS)
        )
      ),
      true
    )

  val logService: DataAccessAlgebra[Future] =
    new DataAccessService(logRepo)

  val logController: DataAccessLogController = new DataAccessLogController(controllerComponents, logService)
}
