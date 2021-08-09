/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import org.slf4j.LoggerFactory
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.{ ExecutionContext, Future }

trait FormTemplateAlgebra[F[_]] {
  def get(id: FormTemplateId): F[FormTemplate]
}

class FormTemplateService(formTemplateRepo: Repo[FormTemplate], formTemplateRawRepo: Repo[FormTemplateRaw])(implicit
  ec: ExecutionContext
) extends Verifier with Rewriter with SubstituteExpressions with FormTemplateAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  def save(formTemplateRaw: FormTemplateRaw): FOpt[Unit] =
    formTemplateRawRepo.upsert(formTemplateRaw)

  def get(id: FormTemplateId): Future[FormTemplate] = formTemplateRepo.get(id.value)

  def get(id: FormTemplateRawId): Future[FormTemplateRaw] =
    formTemplateRawRepo.getDocumentAsJson(id.value).map {
      case jsObject: JsObject => FormTemplateRaw(jsObject)
      case other              => throw new RuntimeException(s"Expected JsObject type, got $other")
    }

  def delete(formTemplateId: FormTemplateId): FOpt[Unit] =
    formTemplateRepo.delete(formTemplateId.value)

  def list(): Future[List[String]] =
    formTemplateRepo
      .projection("_id")
      .map(_.flatMap { jsValue =>
        (jsValue \ "_id").asOpt[String] match {
          case None =>
            logger.error("Failed to extract _id as a String from json: " + jsValue)
            None
          case some => some
        }
      })

  def verifyAndSave(formTemplate: FormTemplate)(expressionsContext: Substitutions): FOpt[Unit] = {
    val substitutedFormTemplate = substituteExpressions(formTemplate, expressionsContext)
    for {
      _                  <- verify(substitutedFormTemplate)
      formTemplateToSave <- rewrite(substitutedFormTemplate)
      _                  <- formTemplateRepo.upsert(mkSpecimen(formTemplateToSave))
      res                <- formTemplateRepo.upsert(formTemplateToSave)
    } yield res
  }
}
