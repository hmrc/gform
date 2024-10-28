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

package uk.gov.hmrc.gform.sharedmodel

import julienrf.json.derived
import play.api.libs.json._
import scala.util.matching.Regex
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, IncludeIf, JsonUtils, LeafExpr, OFormatWithTemplateReadFallback, TemplatePath }

final case class Fetch(path: List[String]) extends AnyVal

object Fetch {
  implicit val format: OFormat[Fetch] = derived.oformat()
}

sealed trait ConstructAttribute extends Product with Serializable

object ConstructAttribute {
  final case class AsIs(value: Fetch) extends ConstructAttribute
  final case class Concat(value: List[Fetch]) extends ConstructAttribute
  // Unlike Concat, Combine does not specify how to render values from the fetches.
  // It only retrieves the data from the fetches
  // and defers the rendering to the service that will utilize the data.
  final case class Combine(value: List[(DataRetrieve.Attribute, Fetch)]) extends ConstructAttribute

  implicit val format: OFormat[ConstructAttribute] = derived.oformat()
}

final case class AttributeInstruction(
  attribute: DataRetrieve.Attribute,
  from: ConstructAttribute
)

object AttributeInstruction {
  implicit val format: OFormat[AttributeInstruction] = derived.oformat()
}

case class DataRetrieveId(value: String) extends AnyVal

object DataRetrieveId {
  implicit val format: Format[DataRetrieveId] =
    JsonUtils.valueClassFormat[DataRetrieveId, String](DataRetrieveId.apply, _.value)

  val idValidation: String = "[_a-zA-Z]\\w*"
  val unanchoredIdValidation: Regex = s"""$idValidation""".r
}

sealed trait Attr extends Product with Serializable {
  val attributes: List[DataRetrieve.Attribute] = this match {
    case Attr.FromObject(insts) => insts.map(_.attribute)
    case Attr.FromArray(insts)  => insts.map(_.attribute)
  }
}

object Attr {
  final case class FromObject(inst: List[AttributeInstruction]) extends Attr
  final case class FromArray(inst: List[AttributeInstruction]) extends Attr

  implicit val format: OFormat[Attr] = derived.oformat()
}

case class DataRetrieve(
  tpe: DataRetrieve.Type,
  id: DataRetrieveId,
  attributes: Attr,
  attrTypeMapping: Map[DataRetrieve.Attribute, DataRetrieve.AttrType],
  params: List[DataRetrieve.ParamExpr],
  `if`: Option[IncludeIf]
)

object DataRetrieve {

  final case class ParamExpr(
    parameter: DataRetrieve.Parameter,
    expr: Expr
  )

  object ParamExpr {
    implicit val format: OFormat[ParamExpr] = derived.oformat()
  }

  sealed trait AttrType extends Product with Serializable

  object AttrType {
    case object String extends AttrType
    case object Integer extends AttrType
    case object Date extends AttrType
    case object BigDecimal extends AttrType

    implicit val format: OFormat[AttrType] = derived.oformat()
  }

  sealed trait ParamType extends Product with Serializable

  object ParamType {
    case object String extends ParamType
    case object Integer extends ParamType

    implicit val format: OFormat[ParamType] = derived.oformat()
  }

  final case class Type(name: String) extends AnyVal

  final case class Attribute(name: String)
  final case class Parameter(name: String, path: List[String], tpe: ParamType = ParamType.String)

  object Type {
    implicit val format: OFormat[Type] = derived.oformat()
  }

  object Attribute {

    val idValidation: String = "[_a-zA-Z]\\w*"
    val unanchoredIdValidation: Regex = s"""$idValidation""".r

    implicit val format: OFormat[Attribute] = derived.oformat()
  }

  object Parameter {
    def apply(name: String): Parameter = Parameter(name, List.empty[String])
    implicit val format: OFormat[Parameter] = derived.oformat()
  }

  val reads: Reads[DataRetrieve] = new Reads[DataRetrieve] {
    override def reads(json: JsValue): JsResult[DataRetrieve] = {
      val dr: Opt[DataRetrieve] = DataRetrieveDefinitions.read(json)
      dr.fold(e => JsError(e.error), r => JsSuccess(r))
    }
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

  def optOption[T](jsValue: JsValue, path: String)(implicit r: Reads[T]): Opt[Option[T]] =
    jsValue \ path match {
      case JsDefined(json) =>
        json
          .validateOpt[T]
          .fold(
            invalid => Left(UnexpectedState(s"Type of value is invalid for attribute '$path' [error=$invalid]")),
            valid => Right(valid)
          )
      case _: JsUndefined => Right(None)
    }

  implicit val format: OFormat[DataRetrieve] = {
    implicit val attrTypeMappingFormat: Format[Map[DataRetrieve.Attribute, DataRetrieve.AttrType]] =
      JsonUtils.formatMap[DataRetrieve.Attribute, DataRetrieve.AttrType](DataRetrieve.Attribute.apply, _.name)
    OFormatWithTemplateReadFallback(reads)
  }

  implicit val leafExprs: LeafExpr[DataRetrieve] = (path: TemplatePath, t: DataRetrieve) =>
    LeafExpr(path + "if", t.`if`)
}

sealed trait RetrieveDataType extends Product with Serializable

object RetrieveDataType {
  case class ObjectType(data: Map[DataRetrieve.Attribute, String]) extends RetrieveDataType
  case class ListType(data: List[Map[DataRetrieve.Attribute, String]]) extends RetrieveDataType
}

case class DataRetrieveResult(id: DataRetrieveId, data: RetrieveDataType, requestParams: JsValue)

object DataRetrieveResult {
  implicit val dataRetrieveSuccessDataFormat: Format[Map[DataRetrieve.Attribute, String]] =
    implicitly[Format[Map[String, String]]]
      .bimap[Map[DataRetrieve.Attribute, String]](
        _.map { case (key, value) =>
          DataRetrieve.Attribute(key) -> value
        },
        _.map { case (key, value) =>
          key.name -> value
        }
      )
  implicit val retrieveDataTypeFormat: Format[RetrieveDataType] = {

    val reads: Reads[RetrieveDataType] = Reads {
      case a: JsArray =>
        implicitly[Reads[List[Map[DataRetrieve.Attribute, String]]]].reads(a).map(RetrieveDataType.ListType)
      case other => implicitly[Reads[Map[DataRetrieve.Attribute, String]]].reads(other).map(RetrieveDataType.ObjectType)
    }

    val writes: Writes[RetrieveDataType] = Writes[RetrieveDataType] {

      case RetrieveDataType.ObjectType(data) => Json.toJson(data)
      case RetrieveDataType.ListType(data)   => Json.toJson(data)
    }

    Format[RetrieveDataType](reads, writes)
  }
  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
