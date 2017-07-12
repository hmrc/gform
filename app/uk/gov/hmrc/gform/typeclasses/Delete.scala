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

package uk.gov.hmrc.gform.typeclasses

import play.api.libs.json.JsObject
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.models.{ DbOperationResult, Form, FormTemplate, Schema }
import uk.gov.hmrc.gform.repositories.{ AbstractRepo, FormRepository, FormTemplateRepository, SchemaRepository }
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.{ ExecutionContext, Future }

trait Delete[T] {

  def apply(selector: JsObject): Future[Opt[DbOperationResult]]
}

object Delete {

  implicit def form(implicit repo: AbstractRepo[Form], ex: ExecutionContext) = new Delete[Form] {
    override def apply(selector: JsObject): Future[Opt[DbOperationResult]] = {
      repo.delete(selector).value
    }
  }

  //  implicit def formTemplate(implicit repo: FormTemplateRepository, ex: ExecutionContext) = new Delete[FormTemplate] {
  //    override def apply(selector: JsObject): Future[Opt[DbOperationResult]] = {
  //      repo.delete(selector).value
  //    }
  //  }

  //  implicit def schema(implicit repo: SchemaRepository, ex: ExecutionContext) = new Update[Schema] {
  //    def apply(selector: JsObject, template: Schema): Future[Opt[DbOperationResult]] = {
  //
  //      repo.delete(selector, template)
  //    }
  //  }
}
