/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.core.utils

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import uk.gov.hmrc.bforms.core.DateExpr

import scala.language.implicitConversions

/**
  * Created by dimitra on 30/03/17.
  */

object DefaultDateFormatter {
  val formatPattern = "yyyy-MM-dd"
  val dateFormatter = DateTimeFormat.forPattern(formatPattern)
}

object DateHelperFunctions {

  import DefaultDateFormatter._

  val currentDay = new DateTime
  val currentYear = currentDay.getYear

  implicit def numericDateExpr(str: String): DateExpr = {
    val values = str.split("-")
    DateExpr(values(2), values(1), values(0))
  }

  implicit def nextDateExpr(str: String): DateExpr = {
    val dateParts = numericDateExpr(str)

    convertToDate(str).isAfter(currentDay.getMillis) match {
      case true => DateExpr(dateParts.day, dateParts.month, currentYear.toString)
      case false => DateExpr(dateParts.day, dateParts.month, (currentYear + 1).toString)
    }
  }

  implicit def lastDateExpr(str: String): DateExpr = {
    val dateParts = numericDateExpr(str)

    convertToDate(str).isAfter(currentDay.getMillis) match {
      case true => DateExpr(dateParts.day, dateParts.month, (currentYear - 1).toString)
      case false => DateExpr(dateParts.day, dateParts.month, currentYear.toString)
    }
  }

  implicit def todayDateExpr: DateExpr = {
    numericDateExpr(dateFormatter.print(currentDay.getMillis))
  }

  def convertToDate(str: String): DateTime = {
    val values = str.split("-")

    val nextDateStr = currentYear.toString + "-" + values(1) + "-" + values(2)

    dateFormatter.parseDateTime(nextDateStr)
  }

}
