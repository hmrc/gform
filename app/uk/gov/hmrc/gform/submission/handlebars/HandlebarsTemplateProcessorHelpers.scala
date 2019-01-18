/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission.handlebars

import uk.gov.hmrc.gform.time.TimeProvider
import java.time.format.DateTimeFormatter

class HandlebarsTemplateProcessorHelpers(timeProvider: TimeProvider = new TimeProvider) {
  def yesNoToEtmpChoice(yesNoChoice: String): CharSequence = ifNotNull(yesNoChoice) {
    case "1" => "0"
    case "0" => "1"
  }

  def dateToEtmpDate(date: String): CharSequence =
    ifNotNull(date) { d =>
      d.substring(0, 4) + d.substring(5, 7) + d.substring(8, 10)
    }

  // Handlebars.java can't deal with a varargs argument in a helper.
  // Neither can it deal with overloading, so we'd need to do something like
  // either2, either3, either4, etc.
  def either(argument1: String, argument2: String): CharSequence =
    List(argument1, argument2).find(_ != null).orNull

  def getCurrentDate(): CharSequence = DateTimeFormatter.BASIC_ISO_DATE.format(timeProvider.localDateTime)

  def getPeriodKey(period: String): CharSequence = getPeriodValue(period, 0)
  def getPeriodFrom(period: String): CharSequence = getPeriodValue(period, 1)
  def getPeriodTo(period: String): CharSequence = getPeriodValue(period, 2)
  private def getPeriodValue(period: String, index: Int): CharSequence = ifNotNull(period) { _.split('|')(index) }

  private def ifNotNull[T, R](t: T)(f: T => R)(implicit ev: Null <:< R): R = Option(t).map(f).orNull
}
