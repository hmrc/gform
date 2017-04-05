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

import java.time._
import java.time.format.DateTimeFormatter
import uk.gov.hmrc.bforms.typeclasses.Now
import uk.gov.hmrc.bforms.core.DateExpr
import scala.language.implicitConversions

/**
 * Created by dimitra on 30/03/17.
 */

object DefaultDateFormatter {
  val formatPattern = "yyyy-MM-dd"
  val dateFormatter = DateTimeFormatter.ofPattern(formatPattern)
}

object DateHelperFunctions {

  import DefaultDateFormatter._

  def numericDateExpr(str: String): DateExpr = {
    val values = str.split("-")
    DateExpr(values(2), values(1), values(0))
  }

  def nextDateExpr(str: String)(implicit now: Now[LocalDate]): DateExpr = {
    val dateParts = numericDateExpr(str)

    convertToDate(str).isAfter(now.apply()) match {
      case true => DateExpr(dateParts.day, dateParts.month, now.apply().getYear.toString)
      case false => DateExpr(dateParts.day, dateParts.month, (now.apply().getYear + 1).toString)
    }
  }

  def lastDateExpr(str: String)(implicit now: Now[LocalDate]): DateExpr = {
    val dateParts = numericDateExpr(str)

    convertToDate(str).isAfter(now.apply()) match {
      case true => DateExpr(dateParts.day, dateParts.month, (now.apply().getYear - 1).toString)
      case false => DateExpr(dateParts.day, dateParts.month, now.apply().getYear.toString)
    }
  }

  def todayDateExpr(implicit now: Now[LocalDate]): DateExpr = {
    numericDateExpr(now.apply().format(dateFormatter))
  }

  def convertToDate(str: String)(implicit now: Now[LocalDate]): LocalDate = {
    val values = str.split("-")

    val nextDateStr = now.apply().getYear.toString + "-" + values(1) + "-" + values(2)

    LocalDate.parse(nextDateStr, dateFormatter)
  }

}
