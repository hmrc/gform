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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.formtemplate.FormTemplatesControllerRequestHandler
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, LocalisedString, formtemplate }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission

case class ExpandedFormTemplate(expandedSection: List[ExpandedSection]) {
  val allFormComponents: List[FormComponent] =
    expandedSection.flatMap(_.expandedFormComponents.flatMap(_.expandedFormComponent))
  val allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)
}

case class FormTemplate(
  _id: FormTemplateId,
  formName: LocalisedString,
  developmentPhase: Option[DevelopmentPhase],
  formCategory: FormCategory,
  draftRetrievalMethod: DraftRetrievalMethod,
  destinations: Destinations,
  authConfig: formtemplate.AuthConfig,
  emailTemplateId: String,
  emailParameters: Option[NonEmptyList[EmailParameter]],
  webChat: Option[WebChat],
  sections: List[Section],
  acknowledgementSection: AcknowledgementSection,
  declarationSection: DeclarationSection,
  parentFormSubmissionRefs: List[FormComponentId],
  GFC579Ready: Option[String],
  languages: AvailableLanguages,
  save4LaterInfoText: Option[Save4LaterInfoText]
) {
  def listAllSections: List[BaseSection] =
    sections ::: List(declarationSection, acknowledgementSection)

  val expandFormTemplate: ExpandedFormTemplate = ExpandedFormTemplate(sections.map(_.expandSection))
}

object FormTemplate {

  import JsonUtils._

  private case class DeprecatedFormTemplateWithDmsSubmission(
    _id: FormTemplateId,
    formName: LocalisedString,
    developmentPhase: Option[DevelopmentPhase],
    formCategory: FormCategory,
    draftRetrievalMethod: DraftRetrievalMethod,
    dmsSubmission: DmsSubmission,
    authConfig: formtemplate.AuthConfig,
    emailTemplateId: String,
    emailParameters: Option[NonEmptyList[EmailParameter]],
    webChat: Option[WebChat],
    sections: List[Section],
    acknowledgementSection: AcknowledgementSection,
    declarationSection: DeclarationSection,
    parentFormSubmissionRefs: Option[List[FormComponentId]],
    GFC579Ready: Option[String],
    languages: AvailableLanguages,
    save4LaterInfoText: Option[Save4LaterInfoText]) {
    def toNewForm: FormTemplate =
      FormTemplate(
        _id: FormTemplateId,
        formName: LocalisedString,
        developmentPhase: Option[DevelopmentPhase],
        formCategory,
        draftRetrievalMethod: DraftRetrievalMethod,
        destinations = dmsSubmission,
        authConfig: formtemplate.AuthConfig,
        emailTemplateId: String,
        emailParameters: Option[NonEmptyList[EmailParameter]],
        webChat: Option[WebChat],
        sections: List[Section],
        acknowledgementSection: AcknowledgementSection,
        declarationSection: DeclarationSection,
        parentFormSubmissionRefs.toList.flatten,
        GFC579Ready: Option[String],
        languages: AvailableLanguages,
        save4LaterInfoText: Option[Save4LaterInfoText]
      )
  }

  private val readForDeprecatedDmsSubmissionVersion: Reads[DeprecatedFormTemplateWithDmsSubmission] =
    Json.reads[DeprecatedFormTemplateWithDmsSubmission]

  private val readForDestinationsVersion: Reads[FormTemplate] =
    Json.reads[FormTemplate]

  val onlyOneOfDmsSubmissionAndDestinationsMustBeDefined =
    JsError(
      """One and only one of FormTemplate.{dmsSubmission, destinations} must be defined. FormTemplate.dmsSubmission is deprecated. Prefer FormTemplate.destinations.""")

  private val reads = Reads[FormTemplate] { json =>
    ((json \ "dmsSubmission").toOption, (json \ "destinations").toOption) match {
      case (None, None)       => onlyOneOfDmsSubmissionAndDestinationsMustBeDefined
      case (None, Some(_))    => readForDestinationsVersion.reads(json)
      case (Some(_), None)    => readForDeprecatedDmsSubmissionVersion.reads(json).map(_.toNewForm)
      case (Some(_), Some(_)) => onlyOneOfDmsSubmissionAndDestinationsMustBeDefined
    }
  }

  implicit val format: OFormat[FormTemplate] = OFormat(reads, derived.owrites[FormTemplate])

  def transformAndReads(json: JsValue): JsResult[FormTemplate] =
    FormTemplatesControllerRequestHandler
      .normaliseJSON(json)
      .flatMap(format.reads)

  def withDeprecatedDmsSubmission(
    _id: FormTemplateId,
    formName: LocalisedString,
    developmentPhase: Option[DevelopmentPhase] = Some(ResearchBanner),
    formCategory: FormCategory,
    draftRetrievalMethod: DraftRetrievalMethod = OnePerUser(ContinueOrDeletePage.Show),
    dmsSubmission: DmsSubmission,
    authConfig: formtemplate.AuthConfig,
    emailTemplateId: String,
    emailParameters: Option[NonEmptyList[EmailParameter]],
    webChat: Option[WebChat],
    sections: List[Section],
    acknowledgementSection: AcknowledgementSection,
    declarationSection: DeclarationSection,
    parentFormSubmissionRefs: Option[List[FormComponentId]],
    GFC579Ready: Option[String] = Some("false"),
    languages: AvailableLanguages = AvailableLanguages.default,
    save4LaterInfoText: Option[Save4LaterInfoText] = None): FormTemplate =
    DeprecatedFormTemplateWithDmsSubmission(
      _id,
      formName,
      developmentPhase,
      formCategory,
      draftRetrievalMethod,
      dmsSubmission,
      authConfig,
      emailTemplateId,
      emailParameters,
      webChat,
      sections,
      acknowledgementSection,
      declarationSection,
      parentFormSubmissionRefs,
      GFC579Ready,
      languages,
      save4LaterInfoText
    ).toNewForm
}
