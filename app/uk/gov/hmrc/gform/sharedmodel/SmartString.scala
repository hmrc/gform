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

import cats.instances.string._
import cats.syntax.eq._
import play.api.libs.functional.Monoid
import play.api.libs.functional.syntax._
import play.api.libs.json.{ Format, JsArray, JsError, JsObject, JsResult, JsString, JsSuccess, JsValue, Reads }
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, Concat, Constant, Expr, ExprWithPath, LeafExpr, OFormatWithTemplateReadFallback, SmartStringIf, TemplatePath }
import uk.gov.hmrc.gform.sharedmodel.booleanParser.booleanExprParser

import scala.util.matching.Regex
import scala.annotation.tailrec

case class SmartString(localised: LocalisedString, interpolations: List[Expr]) {
  def replace(toReplace: String, replaceWith: String): SmartString =
    copy(localised = localised.replace(toReplace, replaceWith))

  def rawValue(implicit l: LangADT): String = localised.value(l)

  def nonEmpty: Boolean = localised.m.nonEmpty && localised.m.forall { case (_: LangADT, value: String) =>
    value.nonEmpty
  }
}

object SmartString {
  val empty: SmartString = SmartString(LocalisedString.empty, Nil)

  implicit val format: Format[SmartString] = OFormatWithTemplateReadFallback(SmartStringTemplateReader.templateReads)

  implicit val leafExprs: LeafExpr[SmartString] = (path: TemplatePath, t: SmartString) =>
    t.interpolations.map(ExprWithPath(path, _))
}

object SmartStringTemplateReader {

  def escape(s: String): String = s.replace("'", "''").replace("{", "'{'").replace("}", "'}'")

  private def interpolationsFieldName = "interpolations"

  private def extractInterpolations(s: String, initialInterpolations: List[Expr]): JsResult[(String, List[Expr])] = {
    val pattern = """(?s)(.*?)(\$\{.*?\})(.*)""".r

    @tailrec
    def recurse(processed: String, unprocessed: String, interpolations: List[Expr]): JsResult[(String, List[Expr])] =
      unprocessed match {
        case pattern(pre, exprWithParentheses, post) =>
          ValueParser.validateWithParser(exprWithParentheses, ValueParser.expr) match {
            case Left(unexpectedState) =>
              JsError(
                s"""Error while parsing "$s". Failed at the expression $exprWithParentheses: ${unexpectedState.toString}"""
              )
            case Right(expr) =>
              val preEscaped = escape(pre)
              recurse(s"$processed$preEscaped{${interpolations.size}}", post, expr :: interpolations)
          }
        case other =>
          val otherEscaped = escape(other)
          JsSuccess((s"$processed$otherEscaped", interpolations.reverse))
      }
    recurse("", s, initialInterpolations.reverse)
  }

  private def readFromTemplateString(value: String) =
    extractInterpolations(value, Nil)
      .map { case (s, i) => SmartString(LocalisedString(Map(LangADT.En -> s)), i) }

  private def readLocalisedStringFromTemplateObject(value: Map[String, JsValue]) =
    LocalisedString.format.reads(JsObject(value.view.filterKeys(_ =!= interpolationsFieldName).toMap))

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
          extractInterpolations(s, ss.interpolations).map { case (stringWithPlaceholders, interpolations) =>
            SmartString(LocalisedString(ss.localised.m + (lang -> stringWithPlaceholders)), interpolations)
          }
        }
    }

  private def buildSmartStringFromLocalisedStringAndInterpolations(
    ls: LocalisedString,
    interpolations: List[Expr]
  ): JsResult[SmartString] =
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

  private def readFromTemplateArray(array: JsArray): JsResult[SmartString] = {
    val jsObjects = array.value.collect { case jsObj: JsObject => jsObj }
    val objects = jsObjects.map { jsObj =>
      toSmartStringConditional(jsObj)
    }.reverse
    objects.headOption
      .map(objects.tail.foldLeft(_)((acc, e) => e |+| acc))
      .getOrElse(JsError("There are no elements in the conditional SmartString"))
      .map(_.smartString)
  }

  private def toSmartStringConditional(jsObj: JsObject): JsResult[SmartStringConditional] = {
    val ss = readFromTemplateObject((jsObj - "includeIf").fields.toMap)
    val booleanExprResult = (jsObj \ "includeIf").asOpt[JsString]
    val booleanExpr = booleanExprResult.map { b =>
      booleanExprParser(b) match {
        case JsError(unexpectedState) =>
          JsError(s"Failed to parse includeIf: ${unexpectedState.toString}")
        case otherwise => otherwise
      }
    } match {
      case Some(JsSuccess(booleanExpr, _)) => JsSuccess(Some(booleanExpr))
      case Some(JsError(unexpectedState))  => JsError(s"Failed to parse includeIf: ${unexpectedState.toString}")
      case None                            => JsSuccess(None)
    }
    (ss, booleanExpr) match {
      case (JsSuccess(pp, _), JsSuccess(b, _)) => JsSuccess(SmartStringConditional(pp, b))
      case (JsError(error), JsSuccess(_, _))   => JsError(error)
      case (JsSuccess(_, _), JsError(error))   => JsError(error)
      case (JsError(error1), JsError(error2))  => JsError(error1.toString + "\n" + error2.toString)
    }

  }

  val templateReads: Reads[SmartString] = new Reads[SmartString] {
    override def reads(json: JsValue): JsResult[SmartString] = json match {
      case JsString(value) => readFromTemplateString(value)
      case obj: JsObject   => readFromTemplateObject(obj.fields.toMap)
      case arr: JsArray =>
        readFromTemplateArray(arr)
      case _ => JsError(s"Expected a String or an Object while reading a SmartString. Got $json")
    }
  }

}

case class SmartStringConditional(smartString: SmartString, includeIf: Option[BooleanExpr])

object SmartStringConditional {
  def empty: SmartStringConditional = SmartStringConditional(SmartString.empty, None)

  implicit val smartStringConditionalMonoid: Monoid[JsResult[SmartStringConditional]] =
    new Monoid[JsResult[SmartStringConditional]] {
      def append(
        a: JsResult[SmartStringConditional],
        b: JsResult[SmartStringConditional]
      ): JsResult[SmartStringConditional] = (a, b) match {
        case (JsSuccess(va, _), JsSuccess(vb, _)) => appendHelper(va, vb)
        case (e @ JsError(_), JsSuccess(_, _))    => e
        case (JsSuccess(_, _), e @ JsError(_))    => e
        case (JsError(e1), JsError(e2))           => JsError(e1 ++ e2)
      }

      def identity: JsResult[SmartStringConditional] = JsSuccess(SmartStringConditional.empty)
    }

  private def appendHelper(
    ssc1: SmartStringConditional,
    ssc2: SmartStringConditional
  ): JsResult[SmartStringConditional] =
    if (ssc2.includeIf.nonEmpty) {
      JsError(s"An array of objects for a smartString must have at least 1 object without an includeIf")
    } else {
      ssc1.includeIf
        .map { includeIf =>
          val exprMap1 = smartStringToExp(ssc1.smartString)
          val exprMap2 = smartStringToExp(ssc2.smartString)
          val allKeys = exprMap1.keySet ++ exprMap2.keySet
          val combinedMap: Map[LangADT, Expr] = allKeys
            .map(key => (key, exprMap1.get(key), exprMap2.get(key)))
            .map {
              case (key, Some(expr1), Some(expr2)) => (key, SmartStringIf(includeIf, expr1, expr2))
              case (key, Some(expr1), None)        => (key, expr1)
              case (key, None, Some(expr2))        => (key, expr2)
              case (key, None, None)               => (key, Constant(""))
            }
            .toMap
          JsSuccess(SmartStringConditional(toSmartString(combinedMap), None))
        }
        .getOrElse(JsError(s"An array of objects for a smartString must have includeIf expressions defined"))
    }

  private def toSmartString(expMap: Map[LangADT, Expr]): SmartString = {
    val (localised, interpolations) =
      expMap.toList.zipWithIndex
        .foldLeft((Map.empty[LangADT, String], List.empty[Expr])) { case ((m, ls), ((key, expr), index)) =>
          (m + (key -> s"{$index}"), ls :+ expr)
        }
    SmartString(LocalisedString(localised), interpolations)

  }

  private def smartStringToExp(ss: SmartString): Map[LangADT, Expr] = {
    val regex: Regex = "(\\{\\d+\\}|[^{}]+)".r
    val placeholderRegex = "\\{(\\d+)\\}".r
    ss.localised.m.view
      .mapValues(v => regex.findAllIn(v).toList)
      .mapValues(v =>
        v.map {
          case placeholderRegex(n) => n.toIntOption.flatMap(ss.interpolations.lift)
          case otherwise           => Some(Constant(otherwise))

        }
      )
      .mapValues(_.toList.flatten)
      .mapValues {
        case e :: Nil => e.asInstanceOf[Expr]
        case vs       => Concat(vs).asInstanceOf[Expr]
      }
  }.toMap
}
