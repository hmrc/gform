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

package uk.gov.hmrc.gform.models

import cats.Monoid
import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError

import scala.collection.immutable._
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.models._

sealed trait ComponentType {
  def validate(formTemplate: FormTemplate): ValidationResult = this match {
    case Text(_, expr, _) => expr.validate(formTemplate)
    case Date(_, _, _) => Valid
    case Address(_) => Valid
    case Choice(_, _, _, _, _) => Valid
    case Group(fvs, _, _, _, _, _) => ComponentType.validate(fvs.map(_.`type`), formTemplate)
    case FileUpload() => Valid
    case InformationMessage(_, _) => Valid
  }
}

case class Text(constraint: TextConstraint, value: Expr, total: Boolean) extends ComponentType

case class Date(constraintType: DateConstraintType, offset: Offset, value: Option[DateValue]) extends ComponentType

case class Group(
  fields: List[FieldValue],
  orientation: Orientation,
  repeatsMax: Option[Int] = None,
  repeatsMin: Option[Int] = None,
  repeatLabel: Option[String] = None,
  repeatAddAnotherText: Option[String] = None
) extends ComponentType

case class FileUpload() extends ComponentType

case object Date {
  val fields = (id: FieldId) => List("day", "month", "year").map(id.withSuffix)
}

case class Address(international: Boolean) extends ComponentType

case object Address {
  val mandatoryFields = (id: FieldId) => List("street1").map(id.withJSSafeSuffix)
  val optionalFields = (id: FieldId) => List("street2", "street3", "street4", "uk", "postcode", "country").map(id.withJSSafeSuffix)
  val fields = (id: FieldId) => mandatoryFields(id) ++ optionalFields(id)
}

case class InformationMessage(infoType: InfoType, infoText: String) extends ComponentType

sealed trait InfoType
case object StandardInfo extends InfoType
case object LongInfo extends InfoType
case object ImportantInfo extends InfoType
case object BannerInfo extends InfoType
object InfoType {
  implicit val format: OFormat[InfoType] = derived.oformat
}

sealed trait Orientation
case object Vertical extends Orientation
case object Horizontal extends Orientation
object Orientation {

  implicit val format: OFormat[Orientation] = derived.oformat
}

sealed trait ChoiceType
final case object Radio extends ChoiceType
final case object Checkbox extends ChoiceType
final case object YesNo extends ChoiceType
final case object Inline extends ChoiceType

object ChoiceType {
  implicit val format: OFormat[ChoiceType] = derived.oformat
}

case class Choice(`type`: ChoiceType, options: NonEmptyList[String], orientation: Orientation,
  selections: List[Int], optionHelpText: Option[List[String]]) extends ComponentType

object ComponentType {

  implicit def readsNonEmptyList[T: Reads] = Reads[NonEmptyList[T]] { json =>
    Json.fromJson[List[T]](json).flatMap {
      case Nil => JsError(ValidationError(s"Required at least one element. Got: $json"))
      case x :: xs => JsSuccess(NonEmptyList(x, xs))
    }
  }

  implicit def writesNonEmptyList[T: Writes] = Writes[NonEmptyList[T]] { v =>
    JsArray((v.head :: v.tail).map(Json.toJson(_)).toList)
  }

  implicit val format: OFormat[ComponentType] = derived.oformat

  def validate(exprs: List[ComponentType], formTemplate: FormTemplate): ValidationResult = {
    val results = exprs.map(_.validate(formTemplate))
    Monoid[ValidationResult].combineAll(results)
  }
}
