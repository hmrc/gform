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

package uk.gov.hmrc.gform.translation

import cats.implicits._
import io.circe.{ CursorOp, Json }
import io.circe.CursorOp._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, Expr }

sealed trait TranslatableConstant extends Product with Serializable {
  def enString: String = this match {
    case TranslatableConstant.NonTranslated(en) => en.value
    case TranslatableConstant.Translated(en, _) => en.value
  }
}

object TranslatableConstant {

  def apply(en: Constant): TranslatableConstant = TranslatableConstant.NonTranslated(en)

  case class NonTranslated(en: Constant) extends TranslatableConstant
  case class Translated(en: Constant, cy: Constant) extends TranslatableConstant
}

case class PathWithTranslatableConstants(cursorOps: List[CursorOp], constants: List[TranslatableConstant]) {
  val path = CursorOp.opsToPath(cursorOps)
}

case class TopLevelExprData(xs: List[PathWithTranslatableConstants]) {
  import TranslatableConstant._
  def toRows: List[Row] = xs.flatMap { pathWithTranslatableConstant =>
    val path = pathWithTranslatableConstant.path
    val constants = pathWithTranslatableConstant.constants
    constants.map {
      case NonTranslated(en)  => Row(path, en.value, "")
      case Translated(en, cy) => Row(path, en.value, cy.value)
    }
  }

  def forPath(path: String): Option[PathWithTranslatableConstants] = xs.find(_.path === path)

  val paths: Set[String] = xs.map(_.path).toSet

  val cursorOps: List[List[CursorOp]] = xs.map(_.cursorOps)
}

object TopLevelExprData {
  def from(json: Json): TopLevelExprData = {

    val cursorOps = generateExpressionOpHistory(json)
    val textRegex = raw"[a-zA-Z]+".r
    val xs = cursorOps
      .map { history =>
        val aCursor = json.hcursor.replay(history)
        val constants: List[TranslatableConstant] =
          aCursor.as[String].toOption.fold(List.empty[TranslatableConstant]) { expression =>
            val maybeExpr: Opt[Expr] = ValueParser.validateWithParser("${" + expression + "}", ValueParser.expr)
            maybeExpr
              .map { expr =>
                val allConstants: List[TranslatableConstant] = expr.constants
                allConstants.filter {
                  case TranslatableConstant.NonTranslated(en) =>
                    val hasSomeText = textRegex.findFirstIn(en.value)
                    hasSomeText.isDefined
                  case _ => true
                }
              }
              .toOption
              .getOrElse(Nil)
          }
        PathWithTranslatableConstants(history, constants)

      }
      .filter { constants =>
        constants.constants.nonEmpty
      }
    TopLevelExprData(xs)
  }

  def generateExpressionOpHistory(json: Json): List[List[CursorOp]] = {
    val topLevelExpressions = json.hcursor.downField("expressions")
    val topLevelExpressionKeys = topLevelExpressions.keys
    topLevelExpressionKeys
      .fold(List.empty[List[CursorOp]]) { keys =>
        keys.toList.map { key =>
          DownField(key) :: DownField("expressions") :: Nil
        }
      }
  }
}
