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

package uk.gov.hmrc.gform.testonly

import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc._
import reactivemongo.api.DB
import reactivemongo.json.collection.JSONCollection
import uk.gov.hmrc.BuildInfo
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.play.http.NotFoundException

class TestOnlyController(
    mongo: () => DB
) extends BaseController {

  lazy val formTemplates = mongo().collection[JSONCollection]("formTemplate")
  def removeTemplates() = Action.async { implicit request =>
    println("purging mongo database ....")
    formTemplates.drop().map(_ => Results.Ok("Mongo purged")).recover {

      case e =>
        e.printStackTrace()
        Results.InternalServerError(e.toString)
    }
  }

  def buildInfo() = Action { r =>
    Results.Ok(
      Json.toJson(BuildInfo.toMap.mapValues(_.toString))
    )

  }

  def config() = Action { r =>
    val result: JsValue = Json.parse(
      ConfigFactory.load().root().render(ConfigRenderOptions.concise())
    )
    Results.Ok(result)
  }

}

