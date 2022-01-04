/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.core.parsers

import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService.{ IsFalseish, IsTrueish }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OverseasAddress

object OverseasAddressParser {
  def validate(expression: JsValue): Opt[OverseasAddress.Value] =
    expression
      .asOpt[OverseasAddress.Value]
      .toRight(UnexpectedState(s"Cannot convert $expression to OverseasAddress.Value"))

  def mandatoryField(
    string: Option[String],
    obj: OverseasAddress.Configurable.Mandatory
  ): Opt[Option[OverseasAddress.Configurable.Mandatory]] =
    field(string, obj, Some(_), _ => None)

  def optionalField(
    string: Option[String],
    obj: OverseasAddress.Configurable.Optional
  ): Opt[Option[OverseasAddress.Configurable.Optional]] =
    field(string, obj, _ => None, Some(_))

  private def field[A](
    string: Option[String],
    obj: A,
    onTrue: A => Option[A],
    onFalse: A => Option[A]
  ): Opt[Option[A]] =
    string
      .map {
        case IsTrueish()  => Right(onTrue(obj))
        case IsFalseish() => Right(onFalse(obj))
        case unknown      => Left(UnexpectedState(s"Unknown value: $unknown, expected true or false"))
      }
      .getOrElse(Right(None))

}
