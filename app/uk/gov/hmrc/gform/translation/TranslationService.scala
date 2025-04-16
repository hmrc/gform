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

package uk.gov.hmrc.gform.translation

import java.time.Instant
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import play.api.libs.json.JsObject
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateRaw }
import uk.gov.hmrc.gform.translation.audit.{ TranslationAudit, TranslationAuditId, TranslationAuditOverview, TranslationAuditRepository, TranslationResult }

class TranslationService(
  translationAuditRepo: TranslationAuditRepository
)(implicit ec: ExecutionContext) {
  def saveAudit(
    formTemplateId: FormTemplateId,
    originalJson: FormTemplateRaw,
    translatedJson: JsObject,
    workbook: XSSFWorkbook,
    result: TranslationResult
  ): FOpt[Unit] =
    translationAuditRepo.upsert(
      TranslationAudit(
        formTemplateId,
        originalJson.value,
        translatedJson,
        workbook,
        result,
        Instant.now()
      )
    )

  def all(): Future[List[TranslationAuditOverview]] =
    translationAuditRepo.all().map(_.toList)

  def latestAudit(formTemplateId: FormTemplateId): Future[Option[TranslationAuditOverview]] =
    translationAuditRepo.latestAudit(formTemplateId)

  def findAudit(translationAuditId: TranslationAuditId): Future[Option[TranslationAudit]] =
    translationAuditRepo.find(translationAuditId)
}
