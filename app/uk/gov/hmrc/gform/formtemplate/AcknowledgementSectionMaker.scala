/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import play.api.libs.json.{ JsDefined, JsString, JsUndefined, JsValue }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService.{ IsFalseish, IsTrueish }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AcknowledgementSection, AcknowledgementSectionPdf, FormComponent }

class AcknowledgementSectionMaker(json: JsValue) {
  val title: SmartString = (json \ "title").as[SmartString]
  val noPIITitle: Option[SmartString] = (json \ "noPIITitle").asOpt[SmartString]
  val description: Option[SmartString] = (json \ "description").asOpt[SmartString]
  val shortName: Option[SmartString] = (json \ "shortName").asOpt[SmartString]
  val fields: List[FormComponent] = (json \ "fields").as[List[FormComponent]]
  val displayFeedbackLink: Boolean = (json \ "displayFeedbackLink").as[Boolean]

  val showReference: Opt[Boolean] = (json \ "showReference") match {
    case JsDefined(JsString(IsTrueish()))  => Right(true)
    case JsDefined(JsString(IsFalseish())) => Right(false)
    case JsUndefined()                     => Right(true)
    case otherwise                         => Left(UnexpectedState(s"Expected 'true'/'yes' or 'false'/'no' for showReference. Got: $otherwise"))
  }

  val acknowledgementSectionPdf: Option[AcknowledgementSectionPdf] = (json \ "pdf").asOpt[AcknowledgementSectionPdf]
  val acknowledgementSectionInstructionPdf: Option[AcknowledgementSectionPdf] =
    (json \ "instructionPdf").asOpt[AcknowledgementSectionPdf]

  def optAcknowledgementSection(): Opt[AcknowledgementSection] =
    for {
      sr <- showReference
    } yield AcknowledgementSection(
      title,
      description,
      shortName,
      fields,
      sr,
      acknowledgementSectionPdf,
      acknowledgementSectionInstructionPdf,
      displayFeedbackLink
    )

}
