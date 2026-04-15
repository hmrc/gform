/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.translation

import io.circe._
import uk.gov.hmrc.gform.core.parsers.SummaryListParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormCtx, MiniSummaryListValue }

object NonTranslatablePaths {

  def findNonTranslatablePaths(json: Json): List[String] = {

    def loop(cursor: ACursor): List[String] = {

      val currentMatch =
        cursor.get[String]("type").toOption match {
          case Some("miniSummaryList") =>
            val rowsCursor = cursor.downField("rows")
            val rowsSize: Int = rowsCursor.values.map(_.size).getOrElse(0)
            (0 to rowsSize).toList.flatMap { idx =>
              val valueCursor = rowsCursor.downN(idx).downField("value")

              valueCursor.focus.flatMap(_.asString) match {
                case Some(keyValue) =>
                  SummaryListParser.validate(keyValue) match {
                    case Right(MiniSummaryListValue.Reference(FormCtx(_))) =>
                      List(valueCursor.pathString)
                    case _ => Nil
                  }
                case _ => Nil
              }
            }
          case _ => Nil
        }

      val nestedMatches =
        cursor.focus.toList.flatMap { json =>
          json.arrayOrObject(
            List.empty[String],
            arr =>
              arr.zipWithIndex.flatMap { case (_, idx) =>
                loop(cursor.downN(idx))
              }.toList,
            obj =>
              obj.keys.toList.flatMap { key =>
                loop(cursor.downField(key))
              }
          )
        }

      currentMatch ++ nestedMatches
    }

    loop(json.hcursor)
  }
}
