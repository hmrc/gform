/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.libs.json.Json
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.{ ExecutionContext, Future }

class FormTemplateService(formTemplateRepo: FormTemplateRepo, formTemplateRawRepo: FormTemplateRawRepo)
    extends Verifier {

  def save(formTemplateRaw: FormTemplateRaw)(implicit ec: ExecutionContext): FOpt[Unit] =
    formTemplateRawRepo.upsert(formTemplateRaw)

  def get(id: FormTemplateId)(implicit ec: ExecutionContext): Future[FormTemplate] = formTemplateRepo.get(id.value)

  def get(id: FormTemplateRawId)(implicit ec: ExecutionContext): Future[FormTemplateRaw] =
    formTemplateRawRepo.get(id.value)

  def delete(formTemplateId: FormTemplateId)(implicit ec: ExecutionContext): FOpt[Unit] =
    formTemplateRepo.delete(formTemplateId.value)

  def list()(implicit ec: ExecutionContext): Future[List[FormTemplate]] =
    //TODO make it stream
    //TODO constraint it so it will result in no more than N records
    //TODO provide querying functionality
    formTemplateRepo.search(Json.obj())

  def verifyAndSave(formTemplate: FormTemplate)(implicit ec: ExecutionContext): FOpt[Unit] =
    for {
      _   <- verify(formTemplate)
      res <- formTemplateRepo.upsert(formTemplate)
    } yield res
}
