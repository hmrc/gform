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

package uk.gov.hmrc.gform.formtemplate

import play.api.libs.json.{ JsValue, _ }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object FormTemplatePIIRefsHelper {

  private val REGEX_SPECIAL_CHARS = "[{}()\\[\\].+*?^$\\\\|]"

  case class Pos(start: Int, end: Int)
  object Pos {
    implicit val format: Format[Pos] = Json.format[Pos]
  }
  case class PIIDetails(pos: Pos, title: String, fcIds: List[String])
  object PIIDetails {
    implicit val format: Format[PIIDetails] = Json.format[PIIDetails]
  }

  case class PIIDetailsResponse(piis: List[PIIDetails], json: Option[String])
  object PIIDetailsResponse {
    implicit val format: Format[PIIDetailsResponse] = Json.format[PIIDetailsResponse]
  }

  def getTitlesWithPII(jsonStr: String, filters: List[String]): List[PIIDetails] = {
    val json = Json.parse(jsonStr)
    val filtersLower = filters.map(_.toLowerCase)
    val titles = (json \ "sections").map { section =>
      val `type` = (section \ "type").asOpt[String]
      val sectionTitle = getPageTitle(section).toList
      `type` match {
        case Some("addToList") =>
          val defaultPageTitle = (section \ "defaultPage").toOption.flatMap(getPageTitle).toList
          val atlPageTitles = (section \ "pages").map(page => getPageTitle(page).toList)
          sectionTitle ++ defaultPageTitle ++ atlPageTitles
        case _ =>
          sectionTitle
      }
    } ++ (json \ "declarationSection").toOption.flatMap(getPageTitle).toList

    titles.flatMap { title =>
      val smartString = JsString(title).as[SmartString]
      val smartStringFCRefs = smartString.interpolations.flatMap(formComponentRefs)
      val filteredSmartStringFCRefs =
        if (filtersLower.isEmpty) smartStringFCRefs
        else smartStringFCRefs.filter(fcRef => filtersLower.exists(fcRef.toLowerCase.contains))
      if (filteredSmartStringFCRefs.isEmpty) {
        None
      } else {
        val matchPosition: Option[Pos] =
          findMatchingLineRegEx(title).findFirstMatchIn(jsonStr).map(m => Pos(m.start, m.end))
        val startLine: Int =
          matchPosition.map(pos => jsonStr.substring(0, pos.start).count(_ == '\n') + 1).getOrElse(-1)
        val endLine: Int = matchPosition.map(pos => jsonStr.substring(0, pos.end).count(_ == '\n') + 1).getOrElse(-1)
        Some(PIIDetails(Pos(startLine, endLine), title, filteredSmartStringFCRefs))
      }
    }
  }

  /** Regex to find position of the following pattern
    * "title": "<title_value>"
    * OR
    * "title": {
    *    "en": "<title_value_en>"
    */
  private def findMatchingLineRegEx(title: String) =
    s"""(?s)("title")[ \\n]*(:)[ \\n]*(\\{)?[ \\n]*("en")?[ \\n]*(:)?[ \\n]*(")(${escapeRegexChars(
      title
    )})(")""".stripMargin.r

  private def getTitle(jsValue: JsValue) =
    (jsValue \ "title" \ "en").asOpt[String].fold((jsValue \ "title").as[String])(identity)

  private def getMaybeNoPIITitle(jsValue: JsValue): Option[String] =
    (jsValue \ "noPIITitle" \ "en").asOpt[String].orElse((jsValue \ "noPIITitle").asOpt[String])

  private def escapeRegexChars(input: String): String = input.flatMap { c =>
    if (REGEX_SPECIAL_CHARS.contains(c)) List('\\', c) else List(c)
  }

  private def getPageTitle(page: JsValue): Option[String] = {
    val title = getTitle(page)
    val maybeNoPIITitle = getMaybeNoPIITitle(page)
    maybeNoPIITitle.fold[Option[String]](Some(title))(_ => None)
  }

  private def formComponentRefs(expr: Expr): List[String] =
    expr match {
      case Add(field1, field2)                    => formComponentRefs(field1) ++ formComponentRefs(field2)
      case Multiply(field1, field2)               => formComponentRefs(field1) ++ formComponentRefs(field2)
      case Subtraction(field1, field2)            => formComponentRefs(field1) ++ formComponentRefs(field2)
      case IfElse(_, field1, field2)              => formComponentRefs(field1) ++ formComponentRefs(field2)
      case Else(field1, field2)                   => formComponentRefs(field1) ++ formComponentRefs(field2)
      case Sum(field1)                            => formComponentRefs(field1)
      case Count(FormComponentId(value))          => List(value)
      case FormCtx(FormComponentId(value))        => List(value)
      case AddressLens(FormComponentId(value), _) => List(value)
      case Period(dateCtx1, dateCtx2)             => formComponentRefs(dateCtx1) ++ formComponentRefs(dateCtx2)
      case PeriodExt(period, func)                => formComponentRefs(period)
      case ParamCtx(queryParam)                   => Nil
      case AuthCtx(value)                         => List("auth." + value.toString.toLowerCase)
      case UserCtx(value)                         => Nil // TODO
      case Constant(value)                        => Nil
      case PeriodValue(value)                     => Nil
      case HmrcRosmRegistrationCheck(value)       => Nil // TODO
      case LinkCtx(link)                          => Nil
      case FormTemplateCtx(value)                 => Nil
      case DateCtx(value)                         => value.maybeFormCtx.toList.map(_.formComponentId.value)
      case Value                                  => Nil
      case LangCtx                                => Nil
    }

  implicit class JsLookupResultOps(result: JsLookupResult) {
    def map[T](f: JsValue => List[T]): List[T] =
      result.toOption
        .map {
          case JsArray(values) =>
            values.toList.flatMap(f)
          case _ => List.empty
        }
        .getOrElse(List.empty)
  }
}