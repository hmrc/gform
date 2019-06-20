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

package uk.gov.hmrc.gform.submission

import cats.instances.future._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import uk.gov.hmrc.gform.sharedmodel.SubmissionData
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class SubmissionService(
  pdfGeneratorService: PdfGeneratorService,
  formAlgebra: FormAlgebra[FOpt],
  formTemplateService: FormTemplateService,
  destinationsSubmitter: DestinationsSubmitter[FOpt],
  submissionRepo: SubmissionRepo,
  email: EmailService)(implicit ex: ExecutionContext) {

  def submissionWithPdf(
    formId: FormId,
    customerId: String,
    affinityGroup: Option[AffinityGroup],
    submissionData: SubmissionData)(implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
      for {
        form          <- formAlgebra.get(formId)
        submissionInfo = DestinationSubmissionInfo(formId, customerId, affinityGroup, submissionData)
        _             <- formAlgebra.updateDestinationSubmissionInfo(formId, Some(submissionInfo))
        formTemplate  <- fromFutureA(formTemplateService.get(form.formTemplateId))
        _             <- destinationsSubmitter.send(submissionInfo, formTemplate, formAlgebra)
        emailAddress   = email.getEmailAddress(form)
        _             <- fromFutureA(email.sendEmail(emailAddress, formTemplate.emailTemplateId, submissionData.emailParameters))
      } yield ()
      // format: ON

  def submissionDetails(formId: FormId)(implicit ex: ExecutionContext): Future[Submission] =
    submissionRepo.get(formId.value)
}
