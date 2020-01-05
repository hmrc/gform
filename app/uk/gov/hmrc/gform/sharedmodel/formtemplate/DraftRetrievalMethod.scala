/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService.{ IsFalseish, IsTrueish }

sealed trait DraftRetrievalMethod

case class OnePerUser(continueOrDeletePage: ContinueOrDeletePage) extends DraftRetrievalMethod
case class FormAccessCodeForAgents(continueOrDeletePage: ContinueOrDeletePage) extends DraftRetrievalMethod
case object BySubmissionReference extends DraftRetrievalMethod

object DraftRetrievalMethod {
  private case class Helper(value: String, showContinueOrDeletePage: String) {
    def toDraftRetrievalMethod: JsResult[DraftRetrievalMethod] = (value, showContinueOrDeletePage) match {
      case ("onePerUser", ContinueOrDeletePage(conOrDel))              => JsSuccess(OnePerUser(conOrDel))
      case ("formAccessCodeForAgents", ContinueOrDeletePage(conOrDel)) => JsSuccess(FormAccessCodeForAgents(conOrDel))
      case ("submissionReference", IsTrueish())                        => JsSuccess(BySubmissionReference)
      case ("submissionReference", IsFalseish()) =>
        JsError(
          "Failure, showContinueOrDeletePage is invalid in combination with 'draftRetrievalMethod: submissionReference'")
      case (err, _) =>
        JsError(
          s"only three values are allowed for draftRetrievalMethod: either onePerUser, submissionReference or formAccessCodeForAgents; $err is not valid")
    }
  }
  private object Helper {
    val reads = Json.reads[Helper]
  }

  private val templateReads: Reads[DraftRetrievalMethod] = Reads { json =>
    Helper.reads.reads(json).flatMap(_.toDraftRetrievalMethod)
  }

  implicit val format: OFormat[DraftRetrievalMethod] = OFormatWithTemplateReadFallback(templateReads)
}
