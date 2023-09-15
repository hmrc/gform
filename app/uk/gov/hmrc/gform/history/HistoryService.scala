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

package uk.gov.hmrc.gform.history

import cats.syntax.all._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateRaw, FormTemplateRawId }

class HistoryService(
  historyRepo: HistoryRepository
)(implicit ec: ExecutionContext) {

  def allFormTemplateIds(): Future[List[FormTemplateRawId]] = historyRepo.allFormTemplateIds()

  def historyFor(historyId: HistoryId): Future[Option[FormTemplateRaw]] =
    historyRepo.find(historyId).map(_.map(_.content))

  def save(formTemplateHistory: FormTemplateHistory): FOpt[Unit] =
    historyRepo.upsert(formTemplateHistory)

  def formTemplateHistoryOverview(formTemplateRawId: FormTemplateRawId): Future[Seq[HistoryOverview]] =
    historyRepo.overviewFor(formTemplateRawId)

  def overviewWithFilter(historyFilter: HistoryFilter): Future[Seq[HistoryOverviewFull]] =
    historyRepo.overviewWithFilter(historyFilter)

  def previousHistoryId(formTemplateRawId: FormTemplateRawId, historyId: HistoryId): Future[Option[HistoryId]] =
    formTemplateHistoryOverview(formTemplateRawId).map { historyOverviews =>
      historyOverviews
        .sliding(2)
        .toList
        .collectFirst { case previousId :: currentId :: Nil if currentId._id === historyId => previousId._id }
    }

  def nextHistoryId(formTemplateRawId: FormTemplateRawId, historyId: HistoryId): Future[Option[HistoryId]] =
    formTemplateHistoryOverview(formTemplateRawId).map { historyOverviews =>
      historyOverviews
        .sliding(2)
        .toList
        .collectFirst { case currentId :: nextId :: Nil if currentId._id === historyId => nextId._id }
    }
}
