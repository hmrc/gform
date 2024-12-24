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

import cats.Eq
import play.api.libs.json._

sealed trait DraftRetrievalMethod

case class OnePerUser(continueOrDeletePage: ContinueOrDeletePage) extends DraftRetrievalMethod
case class FormAccessCodeForAgents(continueOrDeletePage: ContinueOrDeletePage) extends DraftRetrievalMethod
case object BySubmissionReference extends DraftRetrievalMethod
case class FormAccessCode(continueOrDeletePage: ContinueOrDeletePage) extends DraftRetrievalMethod

case object NotPermitted extends DraftRetrievalMethod

object DraftRetrievalMethod {
  private[formtemplate] case class Helper(value: String, showContinueOrDeletePage: Boolean) {
    def toDraftRetrievalMethod: JsResult[DraftRetrievalMethod] = (value, showContinueOrDeletePage) match {
      case ("onePerUser", conOrDel) => JsSuccess(OnePerUser(ContinueOrDeletePage.fromBoolean(conOrDel)))
      case ("formAccessCodeForAgents", conOrDel) =>
        JsSuccess(FormAccessCodeForAgents(ContinueOrDeletePage.fromBoolean(conOrDel)))
      case ("accessCode", conOrDel) =>
        JsSuccess(FormAccessCode(ContinueOrDeletePage.fromBoolean(conOrDel)))
      case ("notPermitted", _)           => JsSuccess(NotPermitted)
      case ("submissionReference", true) => JsSuccess(BySubmissionReference)
      case ("submissionReference", false) =>
        JsError(
          "Failure, showContinueOrDeletePage is invalid in combination with 'draftRetrievalMethod: submissionReference'"
        )
      case (err, _) =>
        JsError(
          s"only three values are allowed for draftRetrievalMethod: either onePerUser, submissionReference or formAccessCodeForAgents; $err is not valid"
        )
    }

    def toDraftRetrieval: JsResult[DraftRetrievalMethod] = (value, showContinueOrDeletePage) match {
      case ("onePerUser", conOrDel) => JsSuccess(OnePerUser(ContinueOrDeletePage.fromBoolean(conOrDel)))
      case ("accessCode", conOrDel) =>
        JsSuccess(FormAccessCode(ContinueOrDeletePage.fromBoolean(conOrDel)))
      case ("notPermitted", _)           => JsSuccess(NotPermitted)
      case ("submissionReference", true) => JsSuccess(BySubmissionReference)
      case ("submissionReference", false) =>
        JsError(
          "Failure, showContinueOrDeletePage is invalid in combination with 'draftRetrieval: submissionReference'"
        )
      case (err, _) =>
        JsError(
          s"only three values are allowed for draftRetrieval: either onePerUser, submissionReference or accessCode; $err is not valid"
        )
    }
  }
  private[formtemplate] object Helper {
    val reads = Json.reads[Helper]
  }

  private val templateReads: Reads[DraftRetrievalMethod] = Reads { json =>
    Helper.reads.reads(json).flatMap(_.toDraftRetrievalMethod)
  }

  implicit val format: OFormat[DraftRetrievalMethod] = OFormatWithTemplateReadFallback(templateReads)
  implicit val equal: Eq[DraftRetrievalMethod] = Eq.fromUniversalEquals
}
