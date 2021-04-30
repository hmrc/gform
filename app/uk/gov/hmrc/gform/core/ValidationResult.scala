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

package uk.gov.hmrc.gform.core

import cats.{ Eq, Monoid }
import cats.syntax.eq._
import uk.gov.hmrc.gform.exceptions.UnexpectedState

//TODO: use either
sealed trait ValidationResult {
  def isValid: Boolean = this === Valid
  def toEither: Opt[Unit] = this match {
    case Valid           => Right(())
    case Invalid(reason) => Left(UnexpectedState(reason))
  }
}

case object Valid extends ValidationResult
case class Invalid(reason: String) extends ValidationResult

object ValidationResult {

  implicit val validationResultMonoid = new Monoid[ValidationResult] {
    def empty: ValidationResult = Valid
    def combine(x: ValidationResult, y: ValidationResult): ValidationResult = (x, y) match {
      case (Valid, Valid)      => Valid
      case (i @ Invalid(_), _) => i
      case (_, i @ Invalid(_)) => i
    }
  }

  implicit class BooleanToValidationResultSyntax(b: Boolean) {
    def validationResult(invalidReason: => String): ValidationResult = if (b) Valid else Invalid(invalidReason)
  }

  implicit val equal: Eq[ValidationResult] = Eq.fromUniversalEquals
}
