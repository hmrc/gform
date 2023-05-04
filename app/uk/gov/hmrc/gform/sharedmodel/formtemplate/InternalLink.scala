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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait InternalLink extends Product with Serializable

object InternalLink {
  case object PrintAcknowledgementPdf extends InternalLink
  case object PrintSummaryPdf extends InternalLink
  case object NewForm extends InternalLink
  case class NewFormForTemplate(formTemplateId: FormTemplateId) extends InternalLink
  case object NewSession extends InternalLink
  case object SignOut extends InternalLink
  case class PageLink(id: PageId) extends InternalLink
  case class Download(fileName: String) extends InternalLink

  val printAcknowledgementPdf: InternalLink = PrintAcknowledgementPdf
  val printSummaryPdf: InternalLink = PrintSummaryPdf
  val newForm: InternalLink = NewForm
  val newSession: InternalLink = NewSession
  val signOut: InternalLink = SignOut

  implicit val format: OFormat[InternalLink] = derived.oformat()
}
