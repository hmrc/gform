/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.core

import cats.Monoid
import play.api.libs.json._
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.model.{ DmsSubmission, FormField, Section }

sealed trait ValidationResult {
  def toEither: Opt[Unit] = this match {
    case Valid => Right(())
    case Invalid(reason) => Left(InvalidState(reason))
  }
}

case object Valid extends ValidationResult
case class Invalid(reason: String) extends ValidationResult

object ValidationResult {

  implicit val validationResultMonoid = new Monoid[ValidationResult] {
    def empty: ValidationResult = Valid
    def combine(x: ValidationResult, y: ValidationResult): ValidationResult = (x, y) match {
      case (Valid, Valid) => Valid
      case (i @ Invalid(_), _) => i
      case (_, i @ Invalid(_)) => i
    }
  }
}

object TemplateValidator {

  def getMatchingSection(formFields: Seq[FormField], sections: Seq[Section]): Opt[Section] = {
    val formFieldIds: Set[String] = formFields.map(_.id).toSet
    val sectionOpt: Option[Section] = sections.find { section =>
      val sectionIds: Set[String] = section.fields.map(_.id).toSet
      sectionIds == formFieldIds
    }

    sectionOpt match {
      case Some(section) => Right(section)
      case None =>
        val sectionsForPrint = sections.map(_.fields.map(_.id))

        Left(InvalidState(s"""|Cannot find a section corresponding to the formFields
                              |FormFields: $formFieldIds
                              |Sections: $sectionsForPrint""".stripMargin))
    }
  }

  def extractSections(json: JsValue): Opt[Seq[Section]] = extract(json, "sections")

  def extractFormName(json: JsValue): Opt[String] = extract(json, "formName")

  def extractDmsSubmission(json: JsValue): Opt[DmsSubmission] = extract(json, "dmsSubmission")

  def extract[B: Reads](json: JsValue, key: String): Opt[B] = {

    val keyRaw: JsResult[B] = (json \ key).validate[B]

    keyRaw match {
      case JsSuccess(success, _) => Right(success)
      case JsError(error) => Left(InvalidState(s"""|Error when reading '$key' from json:
                                                   |Error: $error
                                                   |Input json: """.stripMargin + Json.prettyPrint(json)))
    }
  }
}
