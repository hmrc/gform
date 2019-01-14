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
import uk.gov.hmrc.gform.sharedmodel.formtemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission

case class ExpandedFormTemplate(expandedSection: List[ExpandedSection]) {
  val allFCs: List[FormComponent] = expandedSection.flatMap(_.expandedFCs.flatMap(_.expandedFC))
  val allFcIds: List[FormComponentId] = allFCs.map(_.id)
}

case class FormTemplate(
  _id: FormTemplateId,
  formName: String,
  description: String,
  developmentPhase: Option[DevelopmentPhase],
  formCategory: Option[FormCategory],
  draftRetrievalMethod: Option[DraftRetrievalMethod],
  submissionReference: Option[TextExpression],
  destinations: Destinations,
  authConfig: formtemplate.AuthConfig,
  emailTemplateId: String,
  emailParameters: List[EmailParameter],
  submitSuccessUrl: String,
  submitErrorUrl: String,
  sections: List[Section],
  acknowledgementSection: AcknowledgementSection,
  declarationSection: DeclarationSection,
  GFC579Ready: Option[String]
) {
  val expandFormTemplate: ExpandedFormTemplate = ExpandedFormTemplate(sections.map(_.expandSection))

  @deprecated(message = "This is replaced by the new destinations field.", since = "20181219")
  def dmsSubmission: DmsSubmission = destinations match {
    case dms: Destinations.DmsSubmission => dms
    case Destinations.DestinationList(NonEmptyList(hmrcDms: Destination.HmrcDms, Nil)) =>
      import hmrcDms._
      DmsSubmission(
        dmsFormId = dmsFormId,
        customerId = customerId,
        classificationType = classificationType,
        businessArea = businessArea)
  }
}

object FormTemplate {
  private case class DeprecatedFormTemplateWithDmsSubmission(
    _id: FormTemplateId,
    formName: String,
    description: String,
    developmentPhase: Option[DevelopmentPhase],
    formCategory: Option[FormCategory],
    draftRetrievalMethod: Option[DraftRetrievalMethod],
    submissionReference: Option[TextExpression],
    dmsSubmission: DmsSubmission,
    authConfig: formtemplate.AuthConfig,
    emailTemplateId: String,
    emailParameters: List[EmailParameter],
    submitSuccessUrl: String,
    submitErrorUrl: String,
    sections: List[Section],
    acknowledgementSection: AcknowledgementSection,
    declarationSection: DeclarationSection,
    GFC579Ready: Option[String]) {
    def toNewForm: FormTemplate =
      FormTemplate(
        _id: FormTemplateId,
        formName: String,
        description: String,
        developmentPhase: Option[DevelopmentPhase],
        formCategory,
        draftRetrievalMethod: Option[DraftRetrievalMethod],
        submissionReference: Option[TextExpression],
        destinations = dmsSubmission,
        authConfig: formtemplate.AuthConfig,
        emailTemplateId: String,
        emailParameters: List[EmailParameter],
        submitSuccessUrl: String,
        submitErrorUrl: String,
        sections: List[Section],
        acknowledgementSection: AcknowledgementSection,
        declarationSection: DeclarationSection,
        GFC579Ready: Option[String]
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

  def withDeprecatedDmsSubmission(
    _id: FormTemplateId,
    formName: String,
    description: String,
    developmentPhase: Option[DevelopmentPhase] = Some(ResearchBanner),
    formCategory: Option[FormCategory],
    draftRetrievalMethod: Option[DraftRetrievalMethod] = Some(OnePerUser),
    submissionReference: Option[TextExpression],
    dmsSubmission: DmsSubmission,
    authConfig: formtemplate.AuthConfig,
    emailTemplateId: String,
    emailParameters: List[EmailParameter],
    submitSuccessUrl: String,
    submitErrorUrl: String,
    sections: List[Section],
    acknowledgementSection: AcknowledgementSection,
    declarationSection: DeclarationSection,
    GFC579Ready: Option[String] = Some("false")): FormTemplate =
    DeprecatedFormTemplateWithDmsSubmission(
      _id,
      formName,
      description,
      developmentPhase,
      formCategory,
      draftRetrievalMethod,
      submissionReference,
      dmsSubmission,
      authConfig,
      emailTemplateId,
      emailParameters,
      submitSuccessUrl,
      submitErrorUrl,
      sections,
      acknowledgementSection,
      declarationSection,
      GFC579Ready
    ).toNewForm
}
