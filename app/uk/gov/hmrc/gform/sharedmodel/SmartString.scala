/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.instances.string._
import cats.syntax.eq._
import play.api.libs.json.{ JsError, JsObject, JsResult, JsString, JsSuccess, JsValue, OFormat, Reads }
import uk.gov.hmrc.gform.core.parsers.{ BasicParsers, ValueParser }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, OFormatWithTemplateReadFallback }

import scala.annotation.tailrec

case class SmartString(localised: LocalisedString, interpolations: List[Expr]) {
  def replace(toReplace: String, replaceWith: String): SmartString =
    copy(localised = localised.replace(toReplace, replaceWith))

  def rawValue(implicit l: LangADT): String = localised.value(l)
}

object SmartString {
  val empty: SmartString = SmartString(LocalisedString.empty, Nil)

  implicit val format: OFormat[SmartString] = OFormatWithTemplateReadFallback(SmartStringTemplateReader.templateReads)
}

object SmartStringTemplateReader {
  private def interpolationsFieldName = "interpolations"

  private def extractInterpolations(s: String, initialInterpolations: List[Expr]): JsResult[(String, List[Expr])] = {
    val pattern = """(.*?)(\$\{.*?\})(.*)""".r

    @tailrec
    def recurse(processed: String, unprocessed: String, interpolations: List[Expr]): JsResult[(String, List[Expr])] =
      unprocessed match {
        case pattern(pre, exprWithParentheses, post) =>
          BasicParsers.validateWithParser(exprWithParentheses, ValueParser.expr) match {
            case Left(unexpectedState) =>
              JsError(
                s"""Error while parsing "$s". Failed at the expression $exprWithParentheses: ${unexpectedState.toString}""")
            case Right(expr) => recurse(s"$processed$pre{${interpolations.size}}", post, expr :: interpolations)
          }
        case other => JsSuccess((s"$processed$other", interpolations.reverse))
      }

    recurse("", s, initialInterpolations.reverse)
  }

  private def readFromTemplateString(value: String) =
    extractInterpolations(value, Nil)
      .map { case (s, i) => SmartString(LocalisedString(Map(LangADT.En -> s)), i) }

  private def readLocalisedStringFromTemplateObject(value: Map[String, JsValue]) =
    LocalisedString.format.reads(JsObject(value.filterKeys(_ =!= interpolationsFieldName)))

  private def readInterpolationsFromTemplateObject(value: Map[String, JsValue]) =
    value
      .get(interpolationsFieldName)
      .map { a =>
        Reads.of[List[Expr]].reads(a)
      }
      .getOrElse(JsSuccess(Nil))

  private def buildSmartStringFromLocalisedStringWithPossibleExpressions(ls: LocalisedString): JsResult[SmartString] =
    ls.m.foldLeft(JsSuccess(SmartString.empty).asInstanceOf[JsResult[SmartString]]) {
      case (acc: JsResult[SmartString], (lang: LangADT, s: String)) =>
        acc.flatMap { ss =>
          extractInterpolations(s, ss.interpolations).map {
            case (stringWithPlaceholders, interpolations) =>
              SmartString(LocalisedString(ss.localised.m + (lang -> stringWithPlaceholders)), interpolations)
          }
        }
    }

  private def buildSmartStringFromLocalisedStringAndInterpolations(
    ls: LocalisedString,
    interpolations: List[Expr]): JsResult[SmartString] =
    buildSmartStringFromLocalisedStringWithPossibleExpressions(ls)
      .flatMap { ss =>
        if (interpolations.isEmpty)
          JsSuccess(ss)
        else if (ss.interpolations.isEmpty)
          JsSuccess(ss.copy(interpolations = interpolations))
        else
          JsError(s"""|SmartStrings can either be constructed with:
                      |  embedded expressions (e.g. $${formComponentId}), OR
                      |  interpolation indexes (e.g. {0}) and a list of interpolation expressions
                      |but not with both.""".stripMargin)
      }

  private def readFromTemplateObject(value: Map[String, JsValue]) =
    for {
      localisedString <- readLocalisedStringFromTemplateObject(value)
      interpolations  <- readInterpolationsFromTemplateObject(value)
      ss              <- buildSmartStringFromLocalisedStringAndInterpolations(localisedString, interpolations)
    } yield ss

  val templateReads: Reads[SmartString] = new Reads[SmartString] {
    override def reads(json: JsValue): JsResult[SmartString] = json match {
      case JsString(value) => readFromTemplateString(value)
      case obj: JsObject   => readFromTemplateObject(obj.fields.toMap)
      case _               => JsError(s"Expected a String or an Object while reading a SmartString. Got $json")
    }
  }
}
