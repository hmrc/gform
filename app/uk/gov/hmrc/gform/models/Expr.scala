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
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.core.{ Valid, Invalid, ValidationResult }

sealed trait Expr {
  def validate(formTemplate: FormTemplate): ValidationResult = {
    val fieldNamesIds: List[FieldId] = formTemplate.sections.flatMap(_.fields.map(_.id))

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = field1.validate(formTemplate)
      val checkField2 = field2.validate(formTemplate)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    this match {
      case Add(field1, field2) => checkFields(field1, field2)
      case Multiply(field1, field2) => checkFields(field1, field2)
      case FormCtx(value) =>
        if (fieldNamesIds.map(_.value).contains(value))
          Valid
        else
          Invalid(s"Form field '$value' is not defined in form template.")
      case AuthCtx(value) => Valid
      case EeittCtx(value) => Valid
      case Constant(_) => Valid
    }
  }
}

final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class FormCtx(value: String) extends Expr
final case class AuthCtx(value: AuthInfo) extends Expr
final case class EeittCtx(value: Eeitt) extends Expr
final case class Constant(value: String) extends Expr

object Expr {
  implicit val format: OFormat[Expr] = derived.oformat
}

sealed trait Operation
final case object Addition extends Operation
final case object Multiplication extends Operation

sealed trait Eeitt
final case object BusinessUser extends Eeitt
final case object Agent extends Eeitt

object Eeitt {
  implicit val format: OFormat[Eeitt] = derived.oformat
}

sealed trait AuthInfo
final case object GG extends AuthInfo
final case object PayeNino extends AuthInfo
final case object SaUtr extends AuthInfo
final case object CtUtr extends AuthInfo

object AuthInfo {
  implicit val format: OFormat[AuthInfo] = derived.oformat
}
