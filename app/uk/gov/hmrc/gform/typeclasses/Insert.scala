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

import play.api.Logger
import uk.gov.hmrc.gform.services._
import play.api.libs.json._
import uk.gov.hmrc.gform.connectors.Save4LaterConnector
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.repositories.{ AbstractRepo, SubmissionRepository }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

trait Insert[T] {
  def apply(selector: JsObject, v: T): Future[Opt[DbOperationResult]]
}

object Insert {

  implicit def form(implicit repo: AbstractRepo[Form], cache: Save4LaterConnector, ex: ExecutionContext, hc: HeaderCarrier) = new Insert[Form] {
    def apply(selector: JsObject, template: Form): Future[Opt[DbOperationResult]] = {
      if (IsEncrypt.is.value)
        cache.put(FormKey(template.formData.userId + template.formData.formTypeId.value, template.formData.version.value), template)
      else
        repo.insert(selector, template)
    }
  }

  implicit def submission(implicit repo: SubmissionRepository, ex: ExecutionContext) = new Insert[Submission] {
    def apply(selector: JsObject, template: Submission): Future[Opt[DbOperationResult]] = {
      repo.insert(selector, template)
    }
  }
}
