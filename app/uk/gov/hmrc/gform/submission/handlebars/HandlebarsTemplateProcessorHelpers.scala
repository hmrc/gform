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

import cats.syntax.option._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.time.TimeProvider
import java.time.format.DateTimeFormatter

import com.fasterxml.jackson.databind.node.ArrayNode
import com.github.jknack.handlebars.{ Handlebars, Options }
import play.api.Logger

class HandlebarsTemplateProcessorHelpers(timeProvider: TimeProvider = new TimeProvider) {
  def yesNoToEtmpChoice(yesNoChoice: Any): CharSequence = log("yesNoChoice", yesNoChoice) {
    ifNotNullAsString(yesNoChoice) {
      case "1"  => condition("0")
      case "0"  => condition("1")
      case "1," => condition("0")
      case "0," => condition("1")
    }
  }

  def dateToEtmpDate(date: Any): CharSequence = log("dateToEtmpDate", date) {
    ifNotNullAsString(date) { d =>
      condition(d.substring(0, 4) + d.substring(5, 7) + d.substring(8, 10))
    }
  }

  def either(first: Any, o: Options): CharSequence = eitherN((first :: o.params.toList): _*)

  // Handlebars.java can't deal with a varargs argument in a helper.
  // Neither can it deal with overloading, so we'd need to do something like
  // either2, either3, either4, etc.
  def either2(arg1: Any, arg2: Any): CharSequence = eitherN(arg1, arg2)
  def either3(arg1: Any, arg2: Any, arg3: Any): CharSequence = eitherN(arg1, arg2, arg3)
  def either4(arg1: Any, arg2: Any, arg3: Any, arg4: Any): CharSequence = eitherN(arg1, arg2, arg3, arg4)
  def either5(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5)
  def either6(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6)
  def either7(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
  def either8(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
  def either9(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any, arg9: Any)
    : CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
  def either10(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
  def either11(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
  def either12(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
  def either13(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
  def either14(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
  def either15(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
  def either16(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any): CharSequence =
    eitherN(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
  def either17(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any): CharSequence =
    eitherN(
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
      arg17)
  def either18(
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any,
    arg18: Any): CharSequence =
    eitherN(
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
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any,
    arg18: Any,
    arg19: Any): CharSequence =
    eitherN(
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
    arg1: Any,
    arg2: Any,
    arg3: Any,
    arg4: Any,
    arg5: Any,
    arg6: Any,
    arg7: Any,
    arg8: Any,
    arg9: Any,
    arg10: Any,
    arg11: Any,
    arg12: Any,
    arg13: Any,
    arg14: Any,
    arg15: Any,
    arg16: Any,
    arg17: Any,
    arg18: Any,
    arg19: Any,
    arg20: Any): CharSequence =
    eitherN(
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

  private[handlebars] def eitherN(args: Any*): CharSequence = log("eitherN", args: _*) {
    condition(args.flatMap(asNotNullString).headOption.orNull)
  }

  def isSuccessCode(code: Integer): CharSequence = log("isSuccessCode", code) { condition(isSuccCode(code)) }
  def isNotSuccessCode(code: Integer): CharSequence = log("isNotSuccessCode", code) { condition(!isSuccCode(code)) }
  private def isSuccCode(code: Integer): Boolean = code != null && code >= 200 && code <= 299

  def desCurrentDate: CharSequence = condition(DateTimeFormatter.ISO_LOCAL_DATE.format(timeProvider.localDateTime))
  def currentDate: CharSequence = condition(DateTimeFormatter.BASIC_ISO_DATE.format(timeProvider.localDateTime))
  def currentTimestamp: CharSequence = condition(DateTimeFormatter.ISO_INSTANT.format(timeProvider.instant))

  def hmrcTaxPeriodKey(s: Any): CharSequence = log("hmrcTaxPeriodKey", s) { hmrcTaxPeriodValue(s, 0) }
  def hmrcTaxPeriodFrom(s: Any): CharSequence = log("hmrcTaxPeriodFrom", s) { hmrcTaxPeriodValue(s, 1) }
  def hmrcTaxPeriodTo(s: Any): CharSequence = log("hmrcTaxPeriodTo", s) { hmrcTaxPeriodValue(s, 2) }

  private def hmrcTaxPeriodValue(period: Any, index: Int): CharSequence =
    ifNotNullAsString(period) { s =>
      condition(s.toString.split('|')(index))
    }

  def toEtmpLegalStatus(frontEndValue: Any): CharSequence = log("toEtmpLegalStatus", frontEndValue) {
    ifNotNullAsString(frontEndValue) { s =>
      condition(s.toLowerCase match {
        case "sole trader" | "sole proprietor"         => "1"
        case "limited liability partnership"           => "2"
        case "partnership"                             => "3"
        case "unincorporated body" | "local authority" => "5"
        case "trust"                                   => "6"
        case "public corporation" | "limited company"  => "7"
      })
    }
  }

  def toEtmpDeclarationStatus(frontEndValue: Any): CharSequence = log("toEtmpDeclarationStatus", frontEndValue) {
    ifNotNullAsString(frontEndValue) { s =>
      condition(s.toLowerCase match {
        case "authorised official"             => "1"
        case "company secretary"               => "2"
        case "director"                        => "3"
        case "partner"                         => "4"
        case "sole proprietor" | "sole trader" => "5"
        case "trustee"                         => "6"
        case "others"                          => "7"
      })
    }
  }

  def lookup(matchString: CharSequence, o: Options): CharSequence =
    log("lookup", (matchString :: o.params.toList): _*) {
      val key = o.params.collect {
        case v if isNullAsBoolean(v) => null
        case s                       => s.toString
      }.toList

      LookupMatchStringParser(matchString.toString, key)
        .getOrElse(throw new Exception(s"""Attempt to lookup (${key
          .map(v => s"'$v'")
          .mkString(" ")}) failed in "$matchString""""))
    }

  def toDesAddressWithoutPostcodeFromArray(fromFieldBase: String, index: Int, options: Options): CharSequence =
    log("toDesAddressWithoutPostcodeFromArray", (fromFieldBase :: index :: options.params.toList): _*) {
      def get(line: String) = {
        val value = options.context.get(s"$fromFieldBase-$line")
        if (isNullAsBoolean(value)) ""
        else
          value
            .cast[ArrayNode]
            .map { a =>
              if (index < a.size) a.get(index).asText else ""
            }
            .getOrElse(throw new Exception(
              s"Expected $fromFieldBase.$line to be null, or an array, but it was $value of type ${value.getClass}."))
      }

      toCompactedDesAddress(get("street1"), get("street2"), get("street3"), get("street4"))
    }

  def toDesAddressWithoutPostcode(fromFieldBase: String, options: Options): CharSequence =
    log("toDesAddressWithoutPostcode", (fromFieldBase :: options.params.toList): _*) {
      def get(line: String) = {
        val value = options.context.get(s"$fromFieldBase-$line")
        if (isNullAsBoolean(value)) ""
        else
          value
            .cast[String]
            .getOrElse(throw new Exception(
              s"Expected $fromFieldBase.$line to be null, a string or not present, but it was $value of type ${value.getClass}."))
      }

      toCompactedDesAddress(get("street1"), get("street2"), get("street3"), get("street4"))
    }

  private def toCompactedDesAddress(lines: String*): Handlebars.SafeString =
    new Handlebars.SafeString(
      lines
        .filterNot(_.trim.isEmpty)
        .padTo(2, " ")
        .zipWithIndex
        .map { case (l, i) => s""""addressLine${i + 1}": "${condition(l)}"""" }
        .mkString(s",${util.Properties.lineSeparator}"))

  def removeEmptyAndGet(dfltAny: Any, index: Int, options: Options): CharSequence =
    log("removeEmptyAndGet", (dfltAny :: index :: options.params.toList): _*) {
      ifNotNullAsString(dfltAny) { dflt =>
        val params = options.params.drop(1).flatMap(asNotNullString).filterNot(_.isEmpty)

        if (index < params.length) params(index)
        else dflt
      }
    }

  def elementAt(array: ArrayNode, index: Int, options: Options): CharSequence =
    log("elementAt", (array :: index :: options.params.toList): _*) {
      condition {
        if (index < array.size) array.get(index).textValue else ""
      }
    }

  def stripCommas(s: Any): CharSequence = log("stripCommas", s) { ifNotNullAsString(s) { _.replaceAll(",", "") } }

  def not(s: Any): CharSequence = log("not", s) {
    ifNotNullAsString(s) { v =>
      (!asBoolean(v)).toString
    }
  }

  def or(first: Any, options: Options): CharSequence = condition {
    (first :: options.params.toList).filterNot(isNullAsBoolean).map(asBoolean).exists(identity)
  }

  def and(first: Any, options: Options): CharSequence = condition {
    (first :: options.params.toList).filterNot(isNullAsBoolean).map(asBoolean).forall(identity)
  }

  def isNull(s: Any): CharSequence = log("isNull", s) { condition(isNullAsBoolean(s)) }
  def isNotNull(s: Any): CharSequence = log("isNotNull", s) { condition(!isNullAsBoolean(s)) }

  private def ifNotNullAsString(t: Any)(f: String => CharSequence): CharSequence = NullString.ifNotNull(t)(f)

  private def asBoolean(t: Any): Boolean = t match {
    case "true"  => true
    case "false" => false
    case _ =>
      throw new Exception(
        s"Expected 'true' or 'false'. Got $t of type ${if (t != null) t.getClass.getName else "null"}")
  }

  private def condition(v: Any): CharSequence =
    v match {
      case null                     => NullString
      case s: Handlebars.SafeString => s
      case s: CharSequence =>
        new Handlebars.SafeString(s.toString.replaceAll("""\\""", """\\\\""").replaceAll("""'""", """\\'"""))
      case u => u.toString
    }

  private def isNullAsBoolean(v: Any) = NullString.isNull(v)

  private def asNotNullString(a: Any): Option[String] =
    if (isNullAsBoolean(a)) None
    else a.toString.some

  private def log(functionName: String, params: Any*)(f: => CharSequence): CharSequence = {
    Logger.debug(s"$functionName(${logArgs(params: _*)}) called")
    val result = f
    Logger.debug(s"$functionName(${logArgs(params: _*)}) result is: ${show(result)}")
    result
  }

  private def show(v: Any): String = v match {
    case null             => "null"
    case cs: CharSequence => s""""$cs": ${cs.getClass.getSimpleName}"""
    case other            => s"$other: ${other.getClass.getSimpleName}"
  }

  private def logArgs(args: Any*): String = args.map(show).mkString(", ")
}

object NullString extends CharSequence {
  def isNull(t: Any): Boolean = t == null || t == NullString

  def ifNotNull(t: Any)(f: String => CharSequence): CharSequence =
    if (isNull(t)) NullString else f(t.toString)

  override def length(): Int = 4
  override def charAt(index: Int): Char = "null".charAt(index)
  override def subSequence(start: Int, end: Int): CharSequence = "null".subSequence(start, end)

  override def toString(): String = "null"
}
