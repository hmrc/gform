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
  def either2(arg1: String, arg2: String): CharSequence = either(arg1, arg2)
  def either3(arg1: String, arg2: String, arg3: String): CharSequence = either(arg1, arg2, arg3)
  def either4(arg1: String, arg2: String, arg3: String, arg4: String): CharSequence = either(arg1, arg2, arg3, arg4)
  def either5(arg1: String, arg2: String, arg3: String, arg4: String, arg5: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5)
  def either6(arg1: String, arg2: String, arg3: String, arg4: String, arg5: String, arg6: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6)
  def either7(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String): CharSequence = either(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
  def either8(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
  def either9(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
  def either10(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
  def either11(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
  def either12(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
  def either13(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
  def either14(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String,
    arg14: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
  def either15(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String,
    arg14: String,
    arg15: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
  def either16(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String,
    arg14: String,
    arg15: String,
    arg16: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
  def either17(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String,
    arg14: String,
    arg15: String,
    arg16: String,
    arg17: String): CharSequence =
    either(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
  def either18(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String,
    arg14: String,
    arg15: String,
    arg16: String,
    arg17: String,
    arg18: String): CharSequence =
    either(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18)
  def either19(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String,
    arg14: String,
    arg15: String,
    arg16: String,
    arg17: String,
    arg18: String,
    arg19: String): CharSequence =
    either(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18,
      arg19)
  def either20(
    arg1: String,
    arg2: String,
    arg3: String,
    arg4: String,
    arg5: String,
    arg6: String,
    arg7: String,
    arg8: String,
    arg9: String,
    arg10: String,
    arg11: String,
    arg12: String,
    arg13: String,
    arg14: String,
    arg15: String,
    arg16: String,
    arg17: String,
    arg18: String,
    arg19: String,
    arg20: String): CharSequence =
    either(
      arg1,
      arg2,
      arg3,
      arg4,
      arg5,
      arg6,
      arg7,
      arg8,
      arg9,
      arg10,
      arg11,
      arg12,
      arg13,
      arg14,
      arg15,
      arg16,
      arg17,
      arg18,
      arg19,
      arg20)

  private[handlebars] def either(args: String*): CharSequence = args.find(_ != null).orNull

  def isSuccessCode(code: String): CharSequence = isSuccCode(code, "true", "false")
  def isNotSuccessCode(code: String): CharSequence = isSuccCode(code, "false", "true")
  private def isSuccCode(code: String, t: String, f: String): String = ifNotNull(code) { c =>
    if (c.matches("2\\d\\d")) t else f
  }

  def getCurrentDate(): CharSequence = DateTimeFormatter.BASIC_ISO_DATE.format(timeProvider.localDateTime)
  def getCurrentTimestamp(): CharSequence = DateTimeFormatter.ISO_INSTANT.format(timeProvider.instant)

  def getPeriodKey(period: String): CharSequence = getPeriodValue(period, 0)
  def getPeriodFrom(period: String): CharSequence = getPeriodValue(period, 1)
  def getPeriodTo(period: String): CharSequence = getPeriodValue(period, 2)
  private def getPeriodValue(period: String, index: Int): CharSequence = ifNotNull(period) { _.split('|')(index) }

  def toEtmpLegalStatus(frontEndValue: String): CharSequence = ifNotNull(frontEndValue) {
    _.toLowerCase match {
      case "sole trader" | "sole proprietor"         => "1"
      case "limited liability partnership"           => "2"
      case "partnership"                             => "3"
      case "unincorporated body" | "local authority" => "5"
      case "trust"                                   => "6"
      case "public corporation" | "limited company"  => "7"
    }
  }

  def toEtmpDeclarationStatus(frontEndValue: String): CharSequence = ifNotNull(frontEndValue) {
    _.toLowerCase match {
      case "authorised official"             => "1"
      case "company secretary"               => "2"
      case "director"                        => "3"
      case "partner"                         => "4"
      case "sole proprietor" | "sole trader" => "5"
      case "trustee"                         => "6"
      case "others"                          => "7"
    }
  }

  private def ifNotNull[T, R](t: T)(f: T => R)(implicit ev: Null <:< R): R = Option(t).map(f).orNull
}
