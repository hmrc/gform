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

package uk.gov.hmrc.gform.submission.destinations

import org.bson.conversions.Bson
import org.mongodb.scala.model.Filters.{ and, equal, in, notEqual }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.{ PdfContent, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.http.HeaderCarrier

trait DestinationAuditAlgebra[M[_]] {
  def apply(
    destination: Destination,
    handlebarsDestinationResponseStatusCode: Option[Int],
    handlebarsDestinationResponseErrorBody: Option[String],
    formId: FormId,
    summaryHtml: PdfContent,
    submissionReference: SubmissionRef,
    template: FormTemplate,
    model: HandlebarsTemplateProcessorModel
  )(implicit hc: HeaderCarrier): M[Unit]

  def auditForcedFormStatusChange(form: Form)(implicit hc: HeaderCarrier): M[Unit]

  def getLatestForForm(formId: FormId)(implicit hc: HeaderCarrier): M[DestinationAudit]

  def findLatestChildAudits(submissionRef: SubmissionRef): M[List[DestinationAudit]]
}

object DestinationAuditAlgebra {
  def auditRepoFormIdSearch(formId: FormId): Bson = equal("formId", formId.value)

  def auditRepoLatestChildAuditsSearch(parentSubmissionRef: SubmissionRef): Bson =
    and(
      notEqual("submissionRef", parentSubmissionRef.value),
      in("parentFormSubmissionRefs", parentSubmissionRef.value)
    )

  val latestTimestampFirst: Bson = equal("timestamp", -1)
}
