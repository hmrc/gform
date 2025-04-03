/*
 * Copyright 2025 HM Revenue & Customs
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

object ExprMasker {

  def primaryRegex: String = "\\$\\{¦redacted[0-9]+¦}"

  // Using Broken Bar (¦) to form expression which cannot be present
  // in json template
  private def toRedacted(i: Int): String = s"$${¦redacted$i¦}"

  private val exprRegex = "\\$\\{((?s).*?)}".r

  def mask(str: String): (String, Map[Int, String]) = {
    val exprs = exprRegex.findAllIn(str).toSet.toList

    val lookup = exprs.zipWithIndex.map { case (expr, index) => index -> expr }.toMap

    val redacted = exprs.zipWithIndex.foldRight(str) { case ((expr, index), acc) =>
      acc.replace(expr, toRedacted(index))
    }
    (redacted, lookup)
  }

  def unmask(str: String, lookup: Map[Int, String]): String = {
    val exprLookup = lookup.map { case (index, value) =>
      toRedacted(index) -> value
    }
    val exprs = exprRegex.findAllIn(str).toSet.toList

    exprs.foldRight(str) { case (expr, acc) =>
      exprLookup.get(expr).map(originalExpr => acc.replace(expr, originalExpr)).getOrElse(acc)
    }
  }

  def remask(str: String, lookup: Map[Int, String]): String =
    lookup.toList.foldLeft(str) { case (current, (index, expr)) =>
      current.replace(expr, toRedacted(index))
    }
}
