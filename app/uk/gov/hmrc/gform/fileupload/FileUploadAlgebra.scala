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

package uk.gov.hmrc.gform.fileupload
import java.time.LocalDateTime

import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.submission.{ PdfAndXmlSummaries, Submission }
import uk.gov.hmrc.http.HeaderCarrier

trait FileUploadAlgebra[F[_]] {
  def createEnvelope(formTypeId: FormTemplateId, expiryDate: LocalDateTime)(implicit hc: HeaderCarrier): F[EnvelopeId]

  def submitEnvelope(
    submission: Submission,
    summaries: PdfAndXmlSummaries,
    dmsSubmission: DmsSubmission,
    numberOfAttachments: Int)(implicit hc: HeaderCarrier): F[Unit]

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): F[Unit]
}
