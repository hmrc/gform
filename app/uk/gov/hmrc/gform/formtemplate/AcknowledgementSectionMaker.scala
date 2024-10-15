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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import play.api.libs.json.{ JsDefined, JsError, JsFalse, JsResult, JsSuccess, JsTrue, JsUndefined, JsValue }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AcknowledgementSection, FormComponent, PdfCtx }

class AcknowledgementSectionMaker(json: JsValue) {
  val optTitle: Opt[Option[SmartString]] = toOpt((json \ "title").validateOpt[SmartString], "/title")
  val optNoPIITitle: Opt[Option[SmartString]] = toOpt((json \ "noPIITitle").validateOpt[SmartString], "/noPIITitle")
  val description: Option[SmartString] = (json \ "description").asOpt[SmartString]
  val shortName: Option[SmartString] = (json \ "shortName").asOpt[SmartString]
  val fields: List[FormComponent] = (json \ "fields").as[List[FormComponent]]
  val displayFeedbackLink: Boolean = (json \ "displayFeedbackLink").as[Boolean]

  val showReference: Opt[Boolean] = json \ "showReference" match {
    case JsDefined(JsTrue)  => Right(true)
    case JsDefined(JsFalse) => Right(false)
    case JsUndefined()      => Right(true)
    case otherwise          => Left(UnexpectedState(s"Expected 'true' or 'false' for showReference. Got: $otherwise"))

  }
  val showBanner: Opt[Boolean] = json \ "showBanner" match {
    case JsDefined(JsTrue)  => Right(true)
    case JsDefined(JsFalse) => Right(false)
    case JsUndefined()      => Right(true)
    case otherwise          => Left(UnexpectedState(s"Expected 'true' or 'false' for showBanner. Got: $otherwise"))
  }

  val acknowledgementSectionPdf: Option[PdfCtx] = (json \ "pdf").asOpt[PdfCtx]
  val acknowledgementSectionInstructionPdf: Option[PdfCtx] =
    (json \ "instructionPdf").asOpt[PdfCtx]

  private def toOpt[A](result: JsResult[A], pathPrefix: String): Opt[A] =
    result match {
      case JsSuccess(a, _) => a.asRight
      case JsError(errors) =>
        UnexpectedState(
          errors
            .map { case (path, validationErrors) =>
              s"Path: $pathPrefix${path.toString}, Errors: ${validationErrors.map(_.messages.mkString(",")).mkString(",")}"
            }
            .mkString(",")
        ).asLeft
    }

  def optAcknowledgementSection(): Opt[AcknowledgementSection] =
    for {
      sr         <- showReference
      title      <- optTitle
      noPIITitle <- optNoPIITitle
      sb         <- showBanner
    } yield AcknowledgementSection(
      title,
      description,
      shortName,
      fields,
      sr,
      acknowledgementSectionPdf,
      acknowledgementSectionInstructionPdf,
      displayFeedbackLink,
      noPIITitle,
      sb
    )

}
