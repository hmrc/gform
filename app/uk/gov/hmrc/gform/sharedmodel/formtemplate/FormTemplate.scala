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

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.formtemplate.FormTemplatesControllerRequestHandler
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, DataRetrieve, LocalisedString, formtemplate }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations

case class FormTemplate(
  _id: FormTemplateId,
  originalId: FormTemplateId,
  version: FormTemplateVersion,
  legacyFormIds: Option[NonEmptyList[FormTemplateId]],
  formName: LocalisedString,
  developmentPhase: Option[DevelopmentPhase],
  formCategory: FormCategory,
  draftRetrievalMethod: DraftRetrievalMethod,
  draftRetrieval: Option[DraftRetrieval],
  destinations: Destinations,
  authConfig: formtemplate.AuthConfig,
  emailTemplateId: Option[LocalisedEmailTemplateId],
  emailParameters: Option[NonEmptyList[EmailParameter]],
  webChat: Option[WebChat],
  formKind: FormKind,
  parentFormSubmissionRefs: List[FormComponentId],
  languages: AvailableLanguages,
  save4LaterInfoText: Option[Save4LaterInfoText],
  summarySection: SummarySection,
  submitSection: Option[SubmitSection],
  displayHMRCLogo: Boolean,
  allowedFileTypes: AllowedFileTypes,
  fileSizeLimit: Option[Int],
  userResearchUrl: Option[UserResearchUrl],
  referrerConfig: Option[ReferrerConfig],
  emailExpr: Option[Expr],
  accessibilityUrl: Option[AccessibilityUrl],
  exitPages: Option[NonEmptyList[ExitPage]],
  expressionsOutput: Option[ExpressionOutput],
  displayWidth: Option[LayoutDisplayWidth],
  emailCodeParameters: Option[NonEmptyList[EmailCodeParameter]],
  dataRetrieve: Option[NonEmptyList[DataRetrieve]],
  accessiblePdf: Boolean,
  displayAccountHeader: Boolean,
  serviceStartPageUrl: Option[ServiceStartPageUrl],
  downloadPreviousSubmissionPdf: Boolean
) {

  def formComponents[A](predicate: PartialFunction[FormComponent, A]): List[A] =
    formKind.allSections.flatMap { section =>
      section.formComponents(predicate)
    }

  def expandedFormComponentsInMainSections: List[FormComponent] =
    formKind.allSections.flatMap(_.expandedFormComponents())
}

object FormTemplate {

  import JsonUtils._

  implicit val format: OFormat[FormTemplate] = derived.oformat()

  def transformAndReads(json: JsValue): JsResult[FormTemplate] =
    FormTemplatesControllerRequestHandler
      .normaliseJSON(json)
      .flatMap(format.reads)

  implicit val leafExprs: LeafExpr[FormTemplate] = (path: TemplatePath, t: FormTemplate) =>
    LeafExpr(path + "sections", t.formKind) ++
      LeafExpr(path + "emailExpr", t.emailExpr) ++
      leafExprsNoSections.exprs(path, t)

  val leafExprsNoSections: LeafExpr[FormTemplate] = (path: TemplatePath, t: FormTemplate) =>
    LeafExpr(path + "summarySection", t.summarySection) ++
      LeafExpr(path, t.destinations)

}
