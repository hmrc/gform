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
import play.api.libs.json.{ Format, JsArray, JsError, JsObject, JsResult, JsString, JsSuccess, JsValue, Json, Reads }
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, Expr, ExprWithPath, LeafExpr, OFormatWithTemplateReadFallback, TemplatePath }
import uk.gov.hmrc.gform.sharedmodel.booleanParser.booleanExprParser

import scala.annotation.tailrec

sealed trait SmartString {
  def fold[B](f: SmartString.SmartStringBase => B)(g: SmartString.SmartStringCond => B): B =
    this match {
      case n: SmartString.SmartStringBase => f(n)
      case r: SmartString.SmartStringCond => g(r)
    }

  def replace(toReplace: String, replaceWith: String): SmartString =
    fold[SmartString](ssb => SmartString.SmartStringBase(ssb.internal.replace(toReplace, replaceWith)))(ssc =>
      SmartString.SmartStringCond(
        ssc.ifConditions.map { case (cond, ssi) => (cond, ssi.replace(toReplace, replaceWith)) },
        ssc.elseCondition.replace(toReplace, replaceWith)
      )
    )

  def defaultRawValue(implicit l: LangADT): String =
    fold(_.internal.localised.value(l))(_.elseCondition.localised.value(l))

  def internals: List[SmartStringInternal] =
    fold(ssb => List(ssb.internal))(ssc => ssc.ifConditions.map(_._2) :+ ssc.elseCondition)

  def allNonEmpty: Boolean = internals.forall(_.nonEmpty())

  def updateInterpolations(f: Expr => Expr): SmartString =
    fold[SmartString](ssb =>
      SmartString.SmartStringBase(ssb.internal.copy(interpolations = ssb.internal.interpolations.map(f)))
    )(ssc =>
      SmartString.SmartStringCond(
        ssc.ifConditions.map { case (b, ssi) => (b, ssi.copy(interpolations = ssi.interpolations.map(f))) },
        ssc.elseCondition.copy(interpolations = ssc.elseCondition.interpolations.map(f))
      )
    )

  def updateIncludeIfs(f: BooleanExpr => BooleanExpr): SmartString =
    fold[SmartString](identity)(ssc =>
      SmartString.SmartStringCond(
        ssc.ifConditions.map { case (b, ssi) => (f(b), ssi) },
        ssc.elseCondition
      )
    )
}

case class SmartStringInternal(localised: LocalisedString, interpolations: List[Expr]) {
  def rawValue(implicit l: LangADT): String = localised.value(l)
  def replace(toReplace: String, replaceWith: String): SmartStringInternal =
    copy(localised = localised.replace(toReplace, replaceWith))
  def nonEmpty(): Boolean = localised.m.nonEmpty && localised.m.forall { case (_: LangADT, value: String) =>
    value.nonEmpty
  }
}

object SmartStringInternal {
  val empty: SmartStringInternal = SmartStringInternal(LocalisedString.empty, Nil)
  implicit val format: Format[SmartStringInternal] = OFormatWithTemplateReadFallback(
    SmartStringTemplateReader.templateReads
  )
}

object SmartString {
  case class SmartStringBase(internal: SmartStringInternal) extends SmartString
  case class SmartStringCond(ifConditions: List[(BooleanExpr, SmartStringInternal)], elseCondition: SmartStringInternal)
      extends SmartString

  def apply(localised: LocalisedString, interpolations: List[Expr]): SmartString =
    SmartStringBase(SmartStringInternal(localised, interpolations))

  implicit val smartStringReads: Reads[SmartString] = new Reads[SmartString] {
    def reads(json: JsValue): JsResult[SmartString] =
      json match {
        case jsArray: JsArray if jsArray.value.nonEmpty => readFromTemplateArray(jsArray)
        case jsValue @ (_: JsObject | _: JsString) =>
          Json.fromJson[SmartStringInternal](jsValue).map(SmartStringBase(_))
        case _ => JsError("Unkown format for SmartString")
      }
  }

  implicit val smartStringFormat: Format[SmartString] = OFormatWithTemplateReadFallback(
    smartStringReads
  )

  private def readFromTemplateArray(array: JsArray): JsResult[SmartString] = {
    val jsObjects = array.value.collect { case jsObj: JsObject => jsObj }.toList

    jsObjects.reverse match {
      case Nil => JsError("There are no elements in the conditional SmartString")
      case lastObject :: rest =>
        val ifObjects = rest.filterNot {
          case jsObj: JsObject => jsObj.keys.exists(key => key.matches("_+includeIf"))
          case _               => true
        }

        if ((lastObject \ "includeIf").isDefined) {
          JsError(
            s"The last object in the array SmartString objects should not have includeIf field, got: $lastObject"
          )
        } else {
          val ifConditions = ifObjects.reverse.map(jsObj =>
            for {
              b        <- getBooleanExpr(jsObj)
              internal <- Json.fromJson[SmartStringInternal](jsObj)
            } yield (b, internal)
          )
          if (ifConditions.forall(_.isSuccess)) {
            val ifConditions1 = ifConditions.collect { case JsSuccess(value, _) => value }
            val last: JsResult[SmartStringInternal] = Json.fromJson[SmartStringInternal](lastObject)
            for {
              last1 <- last
            } yield ifConditions1 match {
              case Nil   => SmartStringBase(last1)
              case items => SmartStringCond(items, last1)
            }
          } else {
            JsError(ifConditions.collectFirst { case JsError(errors) => errors }.getOrElse(Seq()))
          }
        }
    }
  }

  private def getBooleanExpr(jsObj: JsObject): JsResult[BooleanExpr] =
    (jsObj \ "includeIf").asOpt[JsString] match {
      case None =>
        JsError(
          s"IncludeIf is missing. (Only the last object in the array of SmartString objects doesn't have includeIf): $jsObj"
        )
      case Some(includeIf) => booleanExprParser(includeIf)
    }

  implicit val leafExprs: LeafExpr[SmartString] = (path: TemplatePath, t: SmartString) =>
    t match {
      case SmartStringBase(internal) =>
        internal.interpolations.map(ExprWithPath(path, _))
      case SmartStringCond(ifConditions, elseCondition) =>
        val conditionExprs = ifConditions.flatMap { case (condition, smartString) =>
          LeafExpr(path + "includeIf", condition) ++ smartString.interpolations.map(ExprWithPath(path, _))
        }
        val elseExprs = elseCondition.interpolations.map(ExprWithPath(path, _))
        conditionExprs ++ elseExprs
    }
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
      .map { case (s, i) => SmartStringInternal(LocalisedString(Map(LangADT.En -> s)), i) }

  private def readLocalisedStringFromTemplateObject(value: Map[String, JsValue]) =
    LocalisedString.format.reads(JsObject(value.view.filterKeys(_ =!= interpolationsFieldName).toMap))

  private def readInterpolationsFromTemplateObject(value: Map[String, JsValue]) =
    value
      .get(interpolationsFieldName)
      .map { a =>
        Reads.of[List[Expr]].reads(a)
      }
      .getOrElse(JsSuccess(Nil))

  private def buildSmartStringFromLocalisedStringWithPossibleExpressions(
    ls: LocalisedString
  ): JsResult[SmartStringInternal] =
    ls.m.foldLeft(JsSuccess(SmartStringInternal.empty).asInstanceOf[JsResult[SmartStringInternal]]) {
      case (acc: JsResult[SmartStringInternal], (lang: LangADT, s: String)) =>
        acc.flatMap { ss =>
          extractInterpolations(s, ss.interpolations).map { case (stringWithPlaceholders, interpolations) =>
            SmartStringInternal(LocalisedString(ss.localised.m + (lang -> stringWithPlaceholders)), interpolations)
          }
        }
    }

  private def buildSmartStringFromLocalisedStringAndInterpolations(
    ls: LocalisedString,
    interpolations: List[Expr]
  ): JsResult[SmartStringInternal] =
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

  val templateReads: Reads[SmartStringInternal] = new Reads[SmartStringInternal] {
    override def reads(json: JsValue): JsResult[SmartStringInternal] = json match {
      case JsString(value) => readFromTemplateString(value)
      case obj: JsObject   => readFromTemplateObject(obj.fields.toMap)
      case _               => JsError(s"Expected a String or an Object while reading a SmartString. Got $json")
    }
  }
}
