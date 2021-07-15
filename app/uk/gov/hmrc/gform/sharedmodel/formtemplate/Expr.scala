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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.ExprParsers

sealed trait Expr {
  def rewrite: Expr = this match {
    case Else(Else(l, r), e) => Else(l.rewrite, Else(r.rewrite, e.rewrite).rewrite).rewrite
    case Add(l, r)           => Add(l.rewrite, r.rewrite)
    case Multiply(l, r)      => Multiply(l.rewrite, r.rewrite)
    case Subtraction(l, r)   => Subtraction(l.rewrite, r.rewrite)
    case Sum(l)              => Sum(l.rewrite)
    case otherwise           => otherwise
  }
}
final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class Subtraction(field1: Expr, field2: Expr) extends Expr
final case class Else(field1: Expr, field2: Expr) extends Expr
final case class Sum(field1: Expr) extends Expr
final case class Count(formComponentId: FormComponentId) extends Expr
final case class FormCtx(formComponentId: FormComponentId) extends Expr
final case class AddressLens(formComponentId: FormComponentId, detail: AddressDetail) extends Expr
final case class Period(dateCtx1: Expr, dateCtx2: Expr) extends Expr
sealed trait PeriodFn
object PeriodFn {
  case object Sum extends PeriodFn
  case object TotalMonths extends PeriodFn
  case object Years extends PeriodFn
  case object Months extends PeriodFn
  case object Days extends PeriodFn
  implicit val format: OFormat[PeriodFn] = derived.oformat()
}
final case class PeriodExt(period: Expr, func: PeriodFn) extends Expr

sealed trait AddressDetail {

  import AddressDetail._
  def functionName: String = this match {
    case Line1    => "line1"
    case Line2    => "line2"
    case Line3    => "line3"
    case Line4    => "line4"
    case Postcode => "postcode"
    case Country  => "country"
  }
}

object AddressDetail {
  case object Line1 extends AddressDetail
  case object Line2 extends AddressDetail
  case object Line3 extends AddressDetail
  case object Line4 extends AddressDetail
  case object Postcode extends AddressDetail
  case object Country extends AddressDetail

  implicit val format: OFormat[AddressDetail] = derived.oformat()
}

object FormCtx {
  lazy val readsForTemplateJson: Reads[FormCtx] = Reads {
    case JsString(exprAsStr) =>
      ExprParsers.validateFormCtx(exprAsStr).fold(error => JsError(error.toString), JsSuccess(_))
    case otherwise => JsError(s"Invalid expression. Expected String, got $otherwise")
  }

  implicit val format: OFormat[FormCtx] = OFormatWithTemplateReadFallback(readsForTemplateJson)
}

final case class ParamCtx(queryParam: QueryParam) extends Expr
final case class AuthCtx(value: AuthInfo) extends Expr
final case class UserCtx(value: UserField) extends Expr
final case class Constant(value: String) extends Expr
final case class PeriodValue(value: String) extends Expr
final case class HmrcRosmRegistrationCheck(value: RosmProp) extends Expr
final case class LinkCtx(link: InternalLink) extends Expr
final case class FormTemplateCtx(value: FormTemplateProp) extends Expr
final case class DateCtx(value: DateExpr) extends Expr
final case object Value extends Expr
final case object LangCtx extends Expr

object Expr {
  implicit val format: OFormat[Expr] = derived.oformat()

  implicit val leafExprs: LeafExpr[Expr] = (path: TemplatePath, t: Expr) => List(ExprWithPath(path, t))

}

sealed trait RosmProp extends Product with Serializable
case object RosmSafeId extends RosmProp
case object RosmOrganisationName extends RosmProp
case object RosmOrganisationType extends RosmProp
case object RosmIsAGroup extends RosmProp

object RosmProp {
  implicit val format: OFormat[RosmProp] = derived.oformat()
}

sealed trait UserField

object UserField {
  final case object AffinityGroup extends UserField
  final case class Enrolment(serviceName: ServiceName, identifierName: IdentifierName) extends UserField
  final case object EnrolledIdentifier extends UserField

  implicit val format: OFormat[UserField] = derived.oformat()
}

final case class ServiceName(value: String) extends AnyVal
object ServiceName {
  implicit val format: OFormat[ServiceName] = derived.oformat()
}
final case class IdentifierName(value: String) extends AnyVal
object IdentifierName {
  implicit val format: OFormat[IdentifierName] = derived.oformat()
}

sealed trait AuthInfo
final case object GG extends AuthInfo
final case object PayeNino extends AuthInfo
final case object EmailId extends AuthInfo
final case object SaUtr extends AuthInfo
final case object CtUtr extends AuthInfo

object AuthInfo {
  implicit val format: OFormat[AuthInfo] = derived.oformat()
}

sealed trait FormTemplateProp extends Product with Serializable
object FormTemplateProp {
  case object Id extends FormTemplateProp
  case object SubmissionReference extends FormTemplateProp

  implicit val format: OFormat[FormTemplateProp] = derived.oformat()
}
