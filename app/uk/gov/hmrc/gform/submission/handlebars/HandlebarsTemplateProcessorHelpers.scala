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

import java.text.DecimalFormat

import cats.instances.string._
import cats.syntax.option._
import cats.syntax.eq._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.time.TimeProvider
import java.time.format.DateTimeFormatter
import java.util.Base64

import com.fasterxml.jackson.databind.node.{ ArrayNode, ObjectNode, TextNode }
import com.github.jknack.handlebars.{ Handlebars, Options }
import play.api.Logger
import uk.gov.hmrc.gform.sharedmodel.form._

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

  def toEtmpDate(date: java.util.Map[String, String]): CharSequence =
    log("toEtmpDate", date) {
      ifNotNull(date) { df =>
        extractDateFields(df) { (day, month, year) =>
          condition(s"$year${padLeft(month, 2, '0')}${padLeft(day, 2, '0')}")
        }.getOrElse(NullString)
      }
    }

  def toDesDate(date: java.util.Map[String, String]): CharSequence =
    log("toDesDate", date) {
      ifNotNull(date) { df =>
        extractDateFields(df) { (day, month, year) =>
          condition(s"$year-${padLeft(month, 2, '0')}-${padLeft(day, 2, '0')}")
        }.getOrElse(NullString)
      }
    }

  private def extractDateFields[T](date: java.util.Map[String, String])(f: (String, String, String) => T): Option[T] =
    for {
      day   <- extractNonEmptyStringFieldValue(date, "day")
      month <- extractNonEmptyStringFieldValue(date, "month")
      year  <- extractNonEmptyStringFieldValue(date, "year")
    } yield f(day, month, year)

  private def extractNonEmptyStringFieldValue(date: java.util.Map[String, String], field: String): Option[String] =
    for {
      value <- Option(date.get(field))
      if !value.isEmpty
    } yield value

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
  def currentMonth(): CharSequence = condition(timeProvider.localDateTime.getMonthValue)

  def greaterThan(first: Any, second: Any): CharSequence = log("greaterThan", first, second) {
    ifNotNullAsNumber(first) { f =>
      ifNotNullAsNumber(second) { s =>
        condition(f > s)
      }
    }
  }

  def lessThan(first: Any, second: Any): CharSequence = log("lessThan", first, second) {
    ifNotNullAsNumber(first) { f =>
      ifNotNullAsNumber(second) { s =>
        condition(f < s)
      }
    }
  }

  def equal(first: Any, second: Any): CharSequence = log("equal", first, second) {
    ifNotNullAsNumber(first) { f =>
      ifNotNullAsNumber(second) { s =>
        condition(f == s)
      }
    }
  }

  def isSigned(options: Options): CharSequence = log("isSigned") {
    hasStatus(options, Signed)
  }

  def isAccepted(options: Options): CharSequence = log("isAccepted") {
    hasStatus(options, Accepted)
  }

  def isAccepting(options: Options): CharSequence = log("isAccepting") {
    hasStatus(options, Accepting)
  }

  def isRejecting(options: Options): CharSequence = log("isRejecting") {
    hasStatus(options, Rejecting)
  }

  def isSubmitting(options: Options): CharSequence = log("isSubmitting") {
    hasStatus(options, Submitting)
  }

  private def hasStatus(options: Options, requiredStatus: FormStatus) = {
    val formStatus = options.context.get("formStatus")
    ifNotNullAsString(formStatus) { s =>
      (s === requiredStatus.toString).toString
    }
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

  def `match`(matchString: CharSequence, o: Options): CharSequence =
    log("match", (matchString :: o.params.toList): _*) {
      val key = o.params.collect {
        case v if isNullAsBoolean(v) => null
        case s                       => s.toString
      }.toList

      LookupMatchStringParser(matchString.toString, key)
        .getOrElse(throw new Exception(s"""Attempt to match (${key
          .map(v => s"'$v'")
          .mkString(" ")}) failed in "$matchString""""))
    }

  def toDesAddressWithoutPostcodeFromArray(fromField: ArrayNode, index: Int, options: Options): CharSequence =
    log("toDesAddressWithoutPostcodeFromArray", (fromField :: index :: options.params.toList): _*) {
      def get(line: String) =
        fromField.get(index).cast[ObjectNode].flatMap(_.get(line).cast[TextNode]).map(_.asText()).filterNot(_.isEmpty)

      toCompactedDesAddress(get("street1"), get("street2"), get("street3"), get("street4"))
    }

  def toDesAddressWithoutPostcode(address: java.util.Map[String, String]): CharSequence =
    log("toDesAddressWithoutPostcode", address) {
      def get(line: String) = Option(address.get(line)).map(_.trim).filterNot(_.isEmpty)

      toCompactedDesAddress(get("street1"), get("street2"), get("street3"), get("street4"))
    }

  private def toCompactedDesAddress(lines: Option[String]*): Handlebars.SafeString =
    new Handlebars.SafeString(
      lines
        .collect { case Some(s) => s }
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

  private val etmpParamSequenceFormat = new DecimalFormat("00")
  def toEtmpParamSequence(index: Int): CharSequence = etmpParamSequenceFormat.format(index.toLong + 1)

  def toEtmpTelephoneNumber(tn: Any): CharSequence =
    log("toEtmpTelephoneNumber", tn) {
      ifNotNullAsString(tn) { v =>
        val tv = v.trim
        if (tv.startsWith("+")) "00" + tv.substring(1)
        else tv
      }
    }

  def exists(array: ArrayNode, testValue: Any): CharSequence = log("exists", array, testValue) {
    ifNotNullAsString(testValue) { v =>
      (0 until array.size)
        .map(array.get)
        .map(
          _.cast[TextNode]
            .map(_.asText)
            .getOrElse(throw new Exception(s"exists($array, 'v'): expected elements in the array to be Strings.")))
        .exists(_ === v)
        .toString
    }
  }

  def plus(first: Any, options: Options): CharSequence = log("plus", (first :: options.params.toList): _*) {
    condition((first :: options.params.toList).map(asBigDecimal).foldLeft(BigDecimal(0)) { (acc, d) =>
      acc + d
    })
  }

  def base64Encode(text: Any): CharSequence =
    ifNotNullAsString(text) { v =>
      Base64.getEncoder.encodeToString(v.getBytes("UTF-8"))
    }

  private def asBigDecimal(v: Any): BigDecimal =
    asNotNullString(v).fold(throw new Exception("Expected a number. Got '$v'"))(BigDecimal(_))

  private def ifNotNullAsString(t: Any)(f: String => CharSequence): CharSequence = NullString.ifNotNull(t)(f)

  private def ifNotNullAsNumber(t: Any)(f: Double => CharSequence): CharSequence =
    NullString.ifNotNull(t)(v => f(v.replaceAll(",", "").toDouble))

  private def ifNotNull[T](t: T)(f: T => CharSequence): CharSequence =
    if (t == null) NullString else f(t)

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
        new Handlebars.SafeString(s.toString.replaceAll("""\\""", """\\\\""").replaceAll(""""""", """\\""""))
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

  private def getNonEmpty(o: Options, fieldName: String) = {
    val value = o.get[String](fieldName, "")
    if (value.isEmpty) None else value.some
  }

  private def padLeft(s: String, min: Int, c: Char) =
    if (s.length >= min) s
    else c.toString * (min - s.length) + s
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
