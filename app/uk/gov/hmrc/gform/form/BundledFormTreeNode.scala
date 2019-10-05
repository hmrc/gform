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

package uk.gov.hmrc.gform.form

import cats.Show
import cats.syntax.show._
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class BundledFormTreeNode(formIdData: FormIdData) {
  lazy val submissionRef: SubmissionRef = formIdData match {
    case _: FormIdData.Plain            => throw new IllegalStateException("BundledFormTreeNode must have an AccessCode!")
    case wac: FormIdData.WithAccessCode => SubmissionRef(wac.accessCode.value)
  }

  def formTemplateId: FormTemplateId = formIdData.formTemplateId
  def formId: FormId = formIdData.toFormId
}

object BundledFormTreeNode {
  implicit val show: Show[BundledFormTreeNode] = Show.show(d => d.formIdData.show)
}
