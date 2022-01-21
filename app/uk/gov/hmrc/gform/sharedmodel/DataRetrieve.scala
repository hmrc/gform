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

package uk.gov.hmrc.gform.sharedmodel

import julienrf.json.derived
import play.api.libs.json.{ Format, JsDefined, JsError, JsObject, JsResult, JsSuccess, JsUndefined, JsValue, OFormat, Reads }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, JsonUtils, OFormatWithTemplateReadFallback }

import scala.util.matching.Regex

case class DataRetrieveId(value: String) extends AnyVal

object DataRetrieveId {
  implicit val format: Format[DataRetrieveId] =
    JsonUtils.valueClassFormat[DataRetrieveId, String](DataRetrieveId.apply, _.value)

  val idValidation: String = "[_a-zA-Z]\\w*"
  val unanchoredIdValidation: Regex = s"""$idValidation""".r
}

sealed trait DataRetrieveAttribute {
  def name: String
}

object DataRetrieveAttribute {

  case object IsValid extends DataRetrieveAttribute {
    override def name: String = "isValid"
  }

  case object AccountNumberIsWellFormatted extends DataRetrieveAttribute {
    override def name: String = "accountNumberIsWellFormatted"
  }

  case object SortCodeIsPresentOnEISCD extends DataRetrieveAttribute {
    override def name: String = "sortCodeIsPresentOnEISCD"
  }

  case object SortCodeBankName extends DataRetrieveAttribute {
    override def name: String = "sortCodeBankName"
  }

  case object NonStandardAccountDetailsRequiredForBacs extends DataRetrieveAttribute {
    override def name: String = "nonStandardAccountDetailsRequiredForBacs"
  }

  case object AccountExists extends DataRetrieveAttribute {
    override def name: String = "accountExists"
  }

  case object NameMatches extends DataRetrieveAttribute {
    override def name: String = "nameMatches"
  }

  case object SortCodeSupportsDirectDebit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectDebit"
  }

  case object SortCodeSupportsDirectCredit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectCredit"
  }

  implicit val format: OFormat[DataRetrieveAttribute] = derived.oformat()

  val idValidation: String = "[_a-zA-Z]\\w*"
  val unanchoredIdValidation: Regex = s"""$idValidation""".r

  def fromName(name: String): DataRetrieveAttribute = name match {
    case "isValid"                                  => IsValid
    case "accountNumberIsWellFormatted"             => AccountNumberIsWellFormatted
    case "sortCodeIsPresentOnEISCD"                 => SortCodeIsPresentOnEISCD
    case "sortCodeBankName"                         => SortCodeBankName
    case "nonStandardAccountDetailsRequiredForBacs" => NonStandardAccountDetailsRequiredForBacs
    case "accountExists"                            => AccountExists
    case "nameMatches"                              => NameMatches
    case "sortCodeSupportsDirectDebit"              => SortCodeSupportsDirectDebit
    case "sortCodeSupportsDirectCredit"             => SortCodeSupportsDirectCredit
    case other                                      => throw new IllegalArgumentException(s"Unknown DataRetrieveAttribute name: $other")
  }
}

sealed trait DataRetrieve {
  def id: DataRetrieveId
  def attributes: List[DataRetrieveAttribute]
  def formCtxExprs: List[Expr]
}

object DataRetrieve {

  final case class ValidateBankDetails(override val id: DataRetrieveId, sortCode: Expr, accountNumber: Expr)
      extends DataRetrieve {
    override def attributes: List[DataRetrieveAttribute] = List(DataRetrieveAttribute.IsValid)
    override def formCtxExprs: List[Expr] = List(sortCode, accountNumber)
  }

  final case class BusinessBankAccountExistence(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr,
    companyName: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      AccountNumberIsWellFormatted,
      SortCodeIsPresentOnEISCD,
      SortCodeBankName,
      NonStandardAccountDetailsRequiredForBacs,
      AccountExists,
      NameMatches,
      SortCodeSupportsDirectDebit,
      SortCodeSupportsDirectCredit
    )
    override def formCtxExprs: List[Expr] = List(sortCode, accountNumber, accountNumber)
  }

  val reads: Reads[DataRetrieve] = new Reads[DataRetrieve] {
    override def reads(json: JsValue): JsResult[DataRetrieve] =
      (for {
        typeValue <- opt[String](json, "type")
        idValue   <- opt[String](json, "id")
        dataRetrieve <- typeValue match {
                          case "validateBankDetails" =>
                            for {
                              parameters         <- opt[JsObject](json, "parameters")
                              sortCodeValue      <- opt[String](parameters, "sortCode")
                              accountNumberValue <- opt[String](parameters, "accountNumber")
                              sortCodeExpr       <- ValueParser.validateWithParser(sortCodeValue, ValueParser.expr)
                              accountNumberExpr  <- ValueParser.validateWithParser(accountNumberValue, ValueParser.expr)
                            } yield ValidateBankDetails(DataRetrieveId(idValue), sortCodeExpr, accountNumberExpr)
                          case "businessBankAccountExistence" =>
                            for {
                              parameters         <- opt[JsObject](json, "parameters")
                              sortCodeValue      <- opt[String](parameters, "sortCode")
                              accountNumberValue <- opt[String](parameters, "accountNumber")
                              companyNameValue   <- opt[String](parameters, "companyName")
                              sortCodeExpr       <- ValueParser.validateWithParser(sortCodeValue, ValueParser.expr)
                              accountNumberExpr  <- ValueParser.validateWithParser(accountNumberValue, ValueParser.expr)
                              companyNameExpr    <- ValueParser.validateWithParser(companyNameValue, ValueParser.expr)
                            } yield BusinessBankAccountExistence(
                              DataRetrieveId(idValue),
                              sortCodeExpr,
                              accountNumberExpr,
                              companyNameExpr
                            )
                          case other => Left(UnexpectedState(s"'type' value $other not recognized"))
                        }
      } yield dataRetrieve).fold(e => JsError(e.error), r => JsSuccess(r))
  }

  def opt[T](jsValue: JsValue, path: String)(implicit r: Reads[T]): Opt[T] =
    jsValue \ path match {
      case JsDefined(json) =>
        json
          .validate[T]
          .fold(
            invalid => Left(UnexpectedState(s"Type of value is invalid for attribute '$path' [error=$invalid]")),
            valid => Right(valid)
          )
      case _: JsUndefined => Left(UnexpectedState(s"'$path' attribute missing"))
    }

  implicit val format: OFormat[DataRetrieve] = OFormatWithTemplateReadFallback(reads)
}

case class DataRetrieveResult(id: DataRetrieveId, data: Map[DataRetrieveAttribute, String], requestParams: JsValue)

object DataRetrieveResult {
  implicit val dataRetrieveSuccessDataFormat: Format[Map[DataRetrieveAttribute, String]] =
    implicitly[Format[Map[String, String]]]
      .bimap[Map[DataRetrieveAttribute, String]](
        _.map { case (key, value) =>
          DataRetrieveAttribute.fromName(key) -> value
        },
        _.map { case (key, value) =>
          key.name -> value
        }
      )
  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
