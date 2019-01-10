/*
 * Copyright 2019 HM Revenue & Customs
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

import play.api.libs.json._
import scala.reflect.runtime.universe.TypeTag

object ADTFormat {
  def formatEnumeration[T: Manifest](translations: (String, T)*): Format[T] = {
    val readMap = translations.toMap
    val writeMap = translations.map(_.swap).toMap

    new Format[T] {
      override def writes(o: T): JsValue = JsString(writeMap(o))

      override def reads(json: JsValue): JsResult[T] =
        json match {
          case JsString(value) =>
            readMap.get(value).fold[JsResult[T]](JsError(invalidReadValue(value, translations: _*)))(JsSuccess(_))
          case _ => JsError("Error")
        }
    }
  }

  def formatEnumerationWithDefault[T: Manifest](dflt: T, translations: (String, T)*): Format[T] = {
    val withoutDefault: Format[T] = formatEnumeration(translations: _*)

    new Format[T] {
      override def reads(json: JsValue): JsResult[T] = json match {
        case JsString("") => JsSuccess(dflt)
        case _            => withoutDefault.reads(json)
      }

      override def writes(o: T): JsValue = withoutDefault.writes(o)
    }
  }

  def adtRead[T: TypeTag](
    typeFieldName: String,
    read1: (String, Reads[_ <: T]),
    reads: (String, Reads[_ <: T])*): Reads[T] = {
    val readMap = (read1 :: reads.toList).toMap

    new Reads[T] {
      override def reads(json: JsValue): JsResult[T] =
        (json \ typeFieldName).toOption.map {
          case JsString(tpe) =>
            readMap
              .get(tpe)
              .map(_.reads(json))
              .getOrElse(JsError(invalidTypeValue[T](typeFieldName, tpe, readMap.keySet)))
          case tpe => JsError(invalidTypeValueType[T](typeFieldName, tpe, readMap.keySet))
        } getOrElse JsError(missingTypeFieldValue[T](typeFieldName, readMap.keySet))
    }
  }

  def missingTypeFieldValue[T: TypeTag](typeFieldName: String, valid: Set[String]): String =
    s"""Missing type discriminator field "$typeFieldName" in ${typeName[T]}. Allowed values are ${prettifyList(valid)}."""

  def invalidTypeValueType[T: TypeTag](typeFieldName: String, tpe: JsValue, valid: Set[String]): String =
    s"""Invalid value ($tpe) for type discriminator field "$typeFieldName" of $tpe in ${typeName[T]}. Allowed values are ${prettifyList(
      valid)}."""

  def invalidTypeValue[T: TypeTag](typeFieldName: String, tpe: String, valid: Set[String]): String =
    s"""Invalid value ($tpe) for type discriminator field "$typeFieldName" value of "$tpe" in ${typeName[T]}. Allowed values are ${prettifyList(
      valid)}."""

  def invalidReadValue[T: TypeTag](s: String, translations: (String, T)*): String =
    s"Invalid JSON value ($s) for custom read of ${typeName[T]}. allowed values are ${prettifyList(translations.map(_._1))}."

  private def prettifyList(ss: Iterable[String]): String = ss.toList.sorted.map(s => s""""$s"""").mkString(", ")

  private def typeName[T](implicit typeTag: TypeTag[T]) = typeTag.tpe
}
