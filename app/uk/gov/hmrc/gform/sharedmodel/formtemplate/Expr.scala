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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._
import play.api.libs.functional.syntax._

import uk.gov.hmrc.gform.core.parsers.ExprParsers

sealed trait Expr
final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class Sum(field1: Expr) extends Expr

final case class FormCtx(value: String) extends Expr {
  def toFieldId = FieldId(this.value)
}

object FormCtx {

  lazy val writesFormCtx = Json.writes[FormCtx]
  lazy val reads: Reads[FormCtx] = readsForTemplateJson | readsForMongoJson

  private lazy val readsForMongoJson: Reads[FormCtx] = Json.reads[FormCtx]

  private lazy val readsForTemplateJson: Reads[FormCtx] = Reads { json =>
    exprParser(json)
  }

  private def exprParser(json: JsValue): JsResult[FormCtx] = {
    json match {
      case JsString(exprAsStr) => parse(exprAsStr)
      case otherwise => JsError(s"Invalid expression. Expected String, got $otherwise")
    }
  }

  private def parse(exprAsStr: String): JsResult[FormCtx] =
    ExprParsers.validate(exprAsStr) fold (
      error => JsError(error.toString),
      expr => JsSuccess(expr)
    )

  implicit val format: OFormat[FormCtx] = OFormat(reads, writesFormCtx)
}

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

