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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.{ ExprParsers, ValueParser }
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }
import uk.gov.hmrc.gform.translation.TranslatableConstant

sealed trait Expr {
  def rewrite: Expr = this match {
    case Else(Else(l, r), e) => Else(l.rewrite, Else(r.rewrite, e.rewrite).rewrite).rewrite
    case Add(l, r)           => Add(l.rewrite, r.rewrite)
    case Multiply(l, r)      => Multiply(l.rewrite, r.rewrite)
    case Subtraction(l, r)   => Subtraction(l.rewrite, r.rewrite)
    case Divide(l, r)        => Divide(l.rewrite, r.rewrite)
    case Sum(l)              => Sum(l.rewrite)
    case otherwise           => otherwise
  }

  def ifElses: List[IfElse] = this match {
    case Else(l, r)        => l.ifElses ++ r.ifElses
    case Add(l, r)         => l.ifElses ++ r.ifElses
    case Multiply(l, r)    => l.ifElses ++ r.ifElses
    case Subtraction(l, r) => l.ifElses ++ r.ifElses
    case Divide(l, r)      => l.ifElses ++ r.ifElses
    case Period(l, r)      => l.ifElses ++ r.ifElses
    case Sum(l)            => l.ifElses
    case PeriodExt(p, _)   => p.ifElses
    case i @ IfElse(cond, l, r) =>
      i :: implicitly[LeafExpr[BooleanExpr]]
        .exprs(TemplatePath.root, cond)
        .flatMap(_.expr.ifElses) ++ l.ifElses ++ r.ifElses
    case otherwise => List.empty[IfElse]
  }

  def constants: List[TranslatableConstant] = this match {
    case Else(l, r)        => l.constants ++ r.constants
    case Add(l, r)         => l.constants ++ r.constants
    case Multiply(l, r)    => l.constants ++ r.constants
    case Subtraction(l, r) => l.constants ++ r.constants
    case Divide(l, r)      => l.constants ++ r.constants
    case Period(l, r)      => l.constants ++ r.constants
    case Sum(l)            => l.constants
    case PeriodExt(p, _)   => p.constants
    case IfElse(cond, l, r) =>
      cond match {
        case Equals(LangCtx, Constant("en")) =>
          (l, r) match {
            case (en @ Constant(_), cy @ Constant(_)) => TranslatableConstant.Translated(en, cy) :: Nil
            case _                                    => Nil
          }
        case _ => l.constants ++ r.constants // Can translatable text be in cond ?
      }

    case c @ Constant(_) => TranslatableConstant(c) :: Nil
    case otherwise       => List.empty[TranslatableConstant]
  }
}
final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class Subtraction(field1: Expr, field2: Expr) extends Expr
final case class Divide(field1: Expr, field2: Expr) extends Expr
final case class IfElse(cond: BooleanExpr, field1: Expr, field2: Expr) extends Expr
final case class Else(field1: Expr, field2: Expr) extends Expr
final case class Sum(field1: Expr) extends Expr
final case class Count(formComponentId: FormComponentId) extends Expr
final case class FormCtx(formComponentId: FormComponentId) extends Expr
final case class AddressLens(formComponentId: FormComponentId, detail: AddressDetail) extends Expr
final case class Period(dateCtx1: Expr, dateCtx2: Expr) extends Expr
final case class Size(formComponentId: FormComponentId, index: SizeRefType) extends Expr
final case class Typed(expr: Expr, tpe: ExplicitExprType) extends Expr
final case class NumberedList(formComponentId: FormComponentId) extends Expr
final case class BulletedList(formComponentId: FormComponentId) extends Expr
final case class Concat(exprs: List[Expr]) extends Expr
final case class StringOps(field1: Expr, stringFnc: StringFnc) extends Expr
final case class OptionDataValue(prefix: String, expr: Expr) extends Expr
object OptionDataValue {
  lazy val readsForTemplateJson: Reads[OptionDataValue] = Reads {
    case JsString(exprAsStr) =>
      ValueParser
        .validateWithParser(exprAsStr, ValueParser.optionDataValue)
        .fold(error => JsError(error.toString), JsSuccess(_))
    case otherwise => JsError(s"Invalid expression. Expected string and expr, got $otherwise")
  }

  implicit val format: OFormat[OptionDataValue] = OFormatWithTemplateReadFallback(readsForTemplateJson)
}

sealed trait SizeRefType extends Product with Serializable

object SizeRefType {
  case class IndexBased(index: Int) extends SizeRefType
  case class ValueBased(value: String) extends SizeRefType

  val regex = "[-_$a-zA-Z0-9{}]+".r

  implicit val format: OFormat[SizeRefType] = derived.oformat()
}

sealed trait ExplicitExprType extends Product with Serializable
object ExplicitExprType {
  case object Text extends ExplicitExprType
  case class Sterling(
    roundingMode: RoundingMode
  ) extends ExplicitExprType
  case class Number(
    fractionalDigits: Int,
    roundingMode: RoundingMode
  ) extends ExplicitExprType

  implicit val format: OFormat[ExplicitExprType] = derived.oformat()
}

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
final case class DateFunction(value: DateProjection) extends Expr
final case object Value extends Expr
final case object LangCtx extends Expr
final case class DataRetrieveCtx(id: DataRetrieveId, attribute: DataRetrieve.Attribute) extends Expr
final case class DataRetrieveCount(id: DataRetrieveId) extends Expr
final case class CsvCountryCheck(formComponentId: FormComponentId, column: String) extends Expr
final case class CsvOverseasCountryCheck(formComponentId: FormComponentId, column: String) extends Expr
final case class CsvCountryCountCheck(formComponentId: FormComponentId, column: String, value: String) extends Expr
final case class IndexOf(formComponentId: FormComponentId, index: Int) extends Expr
final case class IndexOfDataRetrieveCtx(ctx: DataRetrieveCtx, index: Int) extends Expr
final case object CountryOfItmpAddress extends Expr

sealed trait DateProjection extends Product with Serializable {
  def dateExpr: DateExpr
}

object DateProjection {

  case class Day(dateExpr: DateExpr) extends DateProjection
  case class Month(dateExpr: DateExpr) extends DateProjection
  case class Year(dateExpr: DateExpr) extends DateProjection

  implicit val format: OFormat[DateProjection] = derived.oformat()
}

object Expr {
  implicit val dataRetrieveCtxFormat: OFormat[DataRetrieveCtx] = derived.oformat()
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
  final case class Enrolment(serviceName: ServiceName, identifierName: IdentifierName, func: Option[UserFieldFunc])
      extends UserField
  final case object EnrolledIdentifier extends UserField

  implicit val format: OFormat[UserField] = derived.oformat()
}

sealed trait UserFieldFunc
object UserFieldFunc {
  case object Count extends UserFieldFunc
  case class Index(i: Int) extends UserFieldFunc

  implicit val format: OFormat[UserFieldFunc] = derived.oformat()
}

final case class ServiceName(value: String) extends AnyVal
object ServiceName {
  implicit val format: OFormat[ServiceName] = derived.oformat()
}
final case class IdentifierName(value: String) extends AnyVal
object IdentifierName {
  implicit val format: OFormat[IdentifierName] = derived.oformat()
}

sealed trait ItmpNameFocus

object ItmpNameFocus {
  case object GivenName extends ItmpNameFocus
  case object MiddleName extends ItmpNameFocus
  case object FamilyName extends ItmpNameFocus

  implicit val format: OFormat[ItmpNameFocus] = derived.oformat()
}

sealed trait AuthInfo

object AuthInfo {

  final case object GG extends AuthInfo
  final case object PayeNino extends AuthInfo
  final case object EmailId extends AuthInfo
  final case object SaUtr extends AuthInfo
  final case object CtUtr extends AuthInfo
  final case object Name extends AuthInfo
  final case object ItmpName extends AuthInfo
  final case class ItmpNameLens(focus: ItmpNameFocus) extends AuthInfo
  final case object ItmpDateOfBirth extends AuthInfo
  final case object ItmpAddress extends AuthInfo

  implicit val format: OFormat[AuthInfo] = derived.oformat()
}

sealed trait FormTemplateProp extends Product with Serializable
object FormTemplateProp {
  case object Id extends FormTemplateProp
  case object SubmissionReference extends FormTemplateProp
  case object FileSizeLimit extends FormTemplateProp

  implicit val format: OFormat[FormTemplateProp] = derived.oformat()
}

sealed trait HmrcTaxPeriodInfo
object HmrcTaxPeriodInfo {
  case object PeriodTo extends HmrcTaxPeriodInfo
  case object PeriodFrom extends HmrcTaxPeriodInfo
  case object PeriodDue extends HmrcTaxPeriodInfo

  implicit val format: OFormat[HmrcTaxPeriodInfo] = derived.oformat()
}

sealed trait LoginInfo

object LoginInfo {
  final case object EmailLogin extends LoginInfo
  final case object GGLogin extends LoginInfo

  implicit val format: OFormat[LoginInfo] = derived.oformat()
}

sealed trait StringFnc
object StringFnc {
  case object Capitalize extends StringFnc
  case object CapitalizeAll extends StringFnc
  case object UpperCase extends StringFnc
  case object LowerCase extends StringFnc
  case object RemoveSpaces extends StringFnc
  case class SubString(beginIndex: Int, endIndex: Int) extends StringFnc
  implicit val format: OFormat[StringFnc] = derived.oformat()
}
