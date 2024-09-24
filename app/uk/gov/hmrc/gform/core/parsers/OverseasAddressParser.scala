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

package uk.gov.hmrc.gform.core.parsers

import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OverseasAddress

object OverseasAddressParser {

  def mandatoryField(
    bool: Option[Boolean],
    obj: OverseasAddress.Configurable.Mandatory
  ): Opt[Option[OverseasAddress.Configurable.Mandatory]] =
    field(bool, obj, Some(_), _ => None)

  def optionalField(
    bool: Option[Boolean],
    obj: OverseasAddress.Configurable.Optional
  ): Opt[Option[OverseasAddress.Configurable.Optional]] =
    field(bool, obj, _ => None, Some(_))

  private def field[A](
    bool: Option[Boolean],
    obj: A,
    onTrue: A => Option[A],
    onFalse: A => Option[A]
  ): Opt[Option[A]] =
    bool
      .map {
        case true  => Right(onTrue(obj))
        case false => Right(onFalse(obj))
      }
      .getOrElse(Right(None))

}
