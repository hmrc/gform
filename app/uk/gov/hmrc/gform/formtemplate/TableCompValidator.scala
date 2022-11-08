/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.Monoid
import cats.implicits._
import uk.gov.hmrc.gform.core.{ Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TableValueRow
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ TableComp, TableValue }
import uk.gov.hmrc.gform.core.ValidationResult.{ BooleanToValidationResultSyntax, validationResultMonoid }

object TableCompValidator {
  private def decrementRowSpan(tableValue: TableValue): TableValue = {
    val decrementedRowspan =
      if (tableValue.rowspan.exists(s => Math.abs(s) > 1)) tableValue.rowspan.map(s => -(Math.abs(s) - 1)) else None

    tableValue.copy(
      value = SmartString(LocalisedString(Map(LangADT.En -> "")), Nil),
      rowspan = decrementedRowspan
    )
  }

  private def rowSpansIndexes(row: TableValueRow): List[(Int, TableValue)] = row.values.zipWithIndex.flatMap {
    case (tableValue, index) =>
      tableValue.rowspan match {
        case Some(s) if Math.abs(s) > 1 => List(index -> tableValue)
        case _                          => List.empty[(Int, TableValue)]
      }
  }

  private def expandColSpans(rows: List[TableValueRow], canExpand: Int => TableValue => Boolean): List[TableValueRow] =
    rows.map { row =>
      val updatedValues = row.values.foldRight(List.empty[TableValue]) { case (currentValue, acc) =>
        currentValue.colspan match {
          case Some(colspan) if canExpand(colspan)(currentValue) =>
            val indexes: List[Int] = List.range(1, colspan)
            val rest = indexes.map { _ =>
              TableValue(
                SmartString(LocalisedString(Map(LangADT.En -> "")), Nil),
                None,
                None,
                None
              )
            }

            currentValue :: rest ++ acc
          case _ => currentValue :: acc

        }

      }
      row.copy(values = updatedValues)
    }

  def normaliseTableComp(table: TableComp): TableComp = {
    val indexes: List[Int] = List.range(1, table.rows.size)

    val expandedColspans = expandColSpans(table.rows, colspan => _ => colspan > 1)

    val expandedRowspans: List[TableValueRow] = indexes.foldLeft(expandedColspans) { case (rows, currentSplit) =>
      val (begin, end): (List[TableValueRow], List[TableValueRow]) = rows.splitAt(currentSplit)

      val currentRow = begin.last
      val endUpdated = end match {
        case nextRow :: restOfRows =>
          val indexes: List[(Int, TableValue)] = rowSpansIndexes(currentRow)
          val updatedTableValues: List[TableValue] = indexes.foldLeft(nextRow.values) {
            case (rowCells, (currentIndex, tableValue)) =>
              val (beginCells, endCells) = rowCells.splitAt(currentIndex)
              val decremented = decrementRowSpan(tableValue)
              beginCells ++ List(decremented) ++ endCells
          }
          val nextRowUpdated = nextRow.copy(values = updatedTableValues)
          nextRowUpdated :: restOfRows
        case Nil => Nil
      }

      begin ++ endUpdated
    }

    val updatedRows = expandColSpans(expandedRowspans, colspan => cell => colspan > 1 && cell.rowspan.exists(_ < 0))

    table.copy(rows = updatedRows)
  }

  private def validateTableDimensions(table: TableComp): List[ValidationResult] = {
    val normalisedComp = normaliseTableComp(table)
    val header = normalisedComp.header
    val rows = normalisedComp.rows
    val dimensionCheck = rows.map(row =>
      (row.values.size === header.size).validationResult(
        s"The number of header columns and row values do not match"
      )
    )

    // This needs to be called only when we are sure dimension of the table are correct
    def rowspanOverflowCheck: List[ValidationResult] = {
      val rowspans: List[List[Int]] = rows.map { row =>
        row.values.map(rowCell => rowCell.rowspan.getOrElse(0))
      }

      rowspans.transpose.map { columnSpans =>
        val totalRowspan = columnSpans.filter(_ > 0).sum
        (totalRowspan <= rows.size).validationResult(
          s"The number of header columns and row values do not match (rowspans exceed number of rows)"
        )
      }
    }

    dimensionCheck ++ (if (dimensionCheck.forall(_ === Valid)) rowspanOverflowCheck else List.empty[ValidationResult])
  }

  private def validateSpan(maybeSpan: Option[Int], name: String): ValidationResult =
    maybeSpan.fold[ValidationResult](Valid) { span =>
      (span > 0).validationResult(
        s"Invalid $name value $span. ${name.capitalize} must be number greater than 0"
      )
    }

  private def validateColspanAndRowspanValues(table: TableComp): List[ValidationResult] =
    table.rows.flatMap { row =>
      row.values.flatMap { cell =>
        List(validateSpan(cell.rowspan, "rowspan"), validateSpan(cell.colspan, "colspan"))
      }
    }

  def validateTableComp(table: TableComp): ValidationResult =
    Monoid.combineAll(
      validateColspanAndRowspanValues(table) ++ validateTableDimensions(table)
    )

}
