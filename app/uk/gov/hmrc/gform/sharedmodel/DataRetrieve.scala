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

package uk.gov.hmrc.gform.sharedmodel

import cats.parse.{ Parser, Rfc5234 }
import julienrf.json.derived
import play.api.libs.json.{ Format, Reads, _ }
import play.api.libs.json.Format._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.{ BasicParsers, ValueParser }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, JsonUtils, OFormatWithTemplateReadFallback }

import scala.util.matching.Regex

case class DataRetrieveId(value: String) extends AnyVal

object DataRetrieveId {
  implicit val format: Format[DataRetrieveId] =
    JsonUtils.valueClassFormat[DataRetrieveId, String](DataRetrieveId.apply, _.value)

  val idValidation: String = "[_a-zA-Z]\\w*"
  val unanchoredIdValidation: Regex = s"""$idValidation""".r
  val unanchoredIdValidationParser: Parser[String] = Parser.failWith("Not implemented")

}

sealed trait DataRetrieveAttribute {
  def exprId: String
}
case object DataRetrieveIsValid extends DataRetrieveAttribute {
  override def exprId: String = "isValid"
}

case object DataRetrieveAttribute {

  implicit val format: OFormat[DataRetrieveAttribute] = derived.oformat()

  val idValidation: String = "[_a-zA-Z]\\w*"
  val unanchoredIdValidation: Regex = s"""$idValidation""".r
  val unanchoredIdValidationparser: Parser[String] =
    ((Rfc5234.alpha | Parser.char('_').map(_ => '_')) ~ Rfc5234.alpha.rep0).map(x => x._1.toString + x._2.mkString(""))

  def fromString(value: String): DataRetrieveAttribute = value match {
    case "DataRetrieveIsValid" => DataRetrieveIsValid
    case other                 => throw new IllegalArgumentException(s"Unknown DataRetrieveAttribute value $other")
  }

  def fromExpr(exprId: String): DataRetrieveAttribute = exprId match {
    case "isValid" => DataRetrieveIsValid
    case other     => throw new IllegalArgumentException(s"Unknown DataRetrieveAttribute value $other")
  }
}

sealed trait DataRetrieve {
  def id: DataRetrieveId
  def attributes: List[DataRetrieveAttribute]
  def formCtxExprs: List[Expr]
}
case class ValidateBank(override val id: DataRetrieveId, sortCode: Expr, accountNumber: Expr) extends DataRetrieve {
  override def attributes: List[DataRetrieveAttribute] = List(DataRetrieveIsValid)
  override def formCtxExprs: List[Expr] = List(sortCode, accountNumber)
}

object DataRetrieve {

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
                              sortCodeExpr       <- BasicParsers.validateWithParser(sortCodeValue, ValueParser.expr)
                              accountNumberExpr  <- BasicParsers.validateWithParser(accountNumberValue, ValueParser.expr)
                            } yield ValidateBank(DataRetrieveId(idValue), sortCodeExpr, accountNumberExpr)
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

sealed trait DataRetrieveResult
case object DataRetrieveNotRequired extends DataRetrieveResult
case class DataRetrieveSuccess(id: DataRetrieveId, data: Map[DataRetrieveAttribute, String]) extends DataRetrieveResult
case object DataRetrieveFailed extends DataRetrieveResult
case object DataRetrieveMissingInput extends DataRetrieveResult

object DataRetrieveResult {
  implicit val dataRetrieveSuccessDataFormat: Format[Map[DataRetrieveAttribute, String]] =
    invariantFunctorFormat.inmap[Map[String, String], Map[DataRetrieveAttribute, String]](
      implicitly[Format[Map[String, String]]],
      map =>
        map.map { case (key, value) =>
          (DataRetrieveAttribute.fromString(key), value)
        },
      map =>
        map.map { case (key, value) =>
          (key.toString, value)
        }
    )

  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
