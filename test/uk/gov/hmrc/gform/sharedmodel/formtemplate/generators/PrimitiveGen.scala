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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import java.time.{ Instant, LocalDate, LocalDateTime, ZoneOffset }

import cats.data.NonEmptyList
import org.scalacheck.Gen

trait PrimitiveGen {
  def nonEmptyAsciiPrintableString: Gen[String] =
    for {
      f <- Gen.asciiPrintableChar
      r <- Gen.asciiPrintableStr
    } yield f.toString + r

  def nonEmptyAlphaNumStrGen: Gen[String] =
    for {
      f <- Gen.alphaNumChar
      r <- Gen.alphaNumStr
    } yield f.toString + r

  def variableNameGen: Gen[String] =
    for {
      f <- Gen.alphaChar
      r <- Gen.alphaNumStr
    } yield f.toString + r

  def booleanGen: Gen[Boolean] = Gen.oneOf(false, true)

  def urlGen: Gen[String] =
    for {
      urlBase     <- urlBaseGen
      contextPath <- urlContextPathGen
    } yield s"$urlBase$contextPath"

  def urlBaseGen: Gen[String] =
    for {
      d1 <- nonEmptyAlphaNumStrGen
      d2 <- nonEmptyAlphaNumStrGen
      d3 <- Gen.oneOf(".co.uk", "com")
    } yield s"https://$d1.$d2.$d3"

  def urlContextPathGen: Gen[String] = nonEmptyAlphaNumStrGen.map(s => s"/$s")

  def zeroOrMoreGen[T](gen: Gen[T]): Gen[List[T]] = Gen.oneOf(
    Gen.const(Nil),
    gen.map(List(_)),
    for {
      t1 <- gen
      t2 <- gen
    } yield List(t1, t2)
  )

  def oneOrMoreGen[T](gen: Gen[T]): Gen[NonEmptyList[T]] = Gen.oneOf(
    gen.map(NonEmptyList.of(_)),
    for {
      t1 <- gen
      t2 <- gen
    } yield NonEmptyList.of(t1, t2)
  )

  def possiblyEmptyMapGen[K, V](keyGen: Gen[K], valueGen: Gen[V]): Gen[Map[K, V]] =
    zeroOrMoreGen {
      for {
        k <- keyGen
        v <- valueGen
      } yield (k, v)
    }.map(_.toMap)

  def localDateTimeGen: Gen[LocalDateTime] =
    for {
      year       <- Gen.chooseNum(1900, 2100)
      month      <- Gen.chooseNum(1, 12)
      dayOfMonth <- Gen.chooseNum(1, 28)
      hour       <- Gen.chooseNum(0, 23)
      minute     <- Gen.chooseNum(0, 59)
      second     <- Gen.chooseNum(0, 59)
    } yield LocalDateTime.of(year, month, dayOfMonth, hour, minute, second)

  def instantGen: Gen[Instant] = {
    val minEpoch = LocalDateTime.of(1970, 1, 1, 0, 0).toInstant(ZoneOffset.UTC).getEpochSecond
    val maxEpoch = LocalDateTime.of(2100, 1, 1, 0, 0).toInstant(ZoneOffset.UTC).getEpochSecond
    Gen.choose(minEpoch, maxEpoch).map(Instant.ofEpochSecond)
  }

  def localDateGen: Gen[LocalDate] =
    for {
      year       <- Gen.chooseNum(1900, 2100)
      month      <- Gen.chooseNum(1, 12)
      dayOfMonth <- Gen.chooseNum(1, 28)
    } yield LocalDate.of(year, month, dayOfMonth)

  // This is taken from DES API
  def desInternationalCountryCodeGen: Gen[String] =
    Gen.oneOf(
      "AD",
      "AE",
      "AF",
      "AG",
      "AI",
      "AL",
      "AM",
      "AN",
      "AO",
      "AQ",
      "AR",
      "AS",
      "AT",
      "AU",
      "AW",
      "AX",
      "AZ",
      "BA",
      "BB",
      "BD",
      "BE",
      "BF",
      "BG",
      "BH",
      "BI",
      "BJ",
      "BM",
      "BN",
      "BQ",
      "BR",
      "BS",
      "BT",
      "BV",
      "BW",
      "BY",
      "BZ",
      "CA",
      "CC",
      "CD",
      "CF",
      "CG",
      "CH",
      "CI",
      "CK",
      "CL",
      "CM",
      "CN",
      "CO",
      "CR",
      "CS",
      "CU",
      "CV",
      "CW",
      "CX",
      "CY",
      "CZ",
      "DE",
      "DJ",
      "DK",
      "DM",
      "DO",
      "DZ",
      "EC",
      "EE",
      "EG",
      "EH",
      "ER",
      "ES",
      "ET",
      "EU",
      "FI",
      "FJ",
      "FK",
      "FM",
      "FO",
      "FR",
      "GA",
      "GD",
      "GE",
      "GF",
      "GG",
      "GH",
      "GI",
      "GL",
      "GM",
      "GN",
      "GP",
      "GQ",
      "GR",
      "GS",
      "GT",
      "GU",
      "GW",
      "GY",
      "HK",
      "HM",
      "HN",
      "HR",
      "HT",
      "HU",
      "ID",
      "IE",
      "IL",
      "IM",
      "IN",
      "IO",
      "IQ",
      "IR",
      "IS",
      "IT",
      "JE",
      "JM",
      "JO",
      "JP",
      "KE",
      "KG",
      "KH",
      "KI",
      "KM",
      "KN",
      "KP",
      "KR",
      "KW",
      "KY",
      "KZ",
      "LA",
      "LB",
      "LC",
      "LI",
      "LK",
      "LR",
      "LS",
      "LT",
      "LU",
      "LV",
      "LY",
      "MA",
      "MC",
      "MD",
      "ME",
      "MF",
      "MG",
      "MH",
      "MK",
      "ML",
      "MM",
      "MN",
      "MO",
      "MP",
      "MQ",
      "MR",
      "MS",
      "MT",
      "MU",
      "MV",
      "MW",
      "MX",
      "MY",
      "MZ",
      "NA",
      "NC",
      "NE",
      "NF",
      "NG",
      "NI",
      "NL",
      "NO",
      "NP",
      "NR",
      "NT",
      "NU",
      "NZ",
      "OM",
      "PA",
      "PE",
      "PF",
      "PG",
      "PH",
      "PK",
      "PL",
      "PM",
      "PN",
      "PR",
      "PS",
      "PT",
      "PW",
      "PY",
      "QA",
      "RE",
      "RO",
      "RS",
      "RU",
      "RW",
      "SA",
      "SB",
      "SC",
      "SD",
      "SE",
      "SG",
      "SH",
      "SI",
      "SJ",
      "SK",
      "SL",
      "SM",
      "SN",
      "SO",
      "SR",
      "SS",
      "ST",
      "SV",
      "SX",
      "SY",
      "SZ",
      "TC",
      "TD",
      "TF",
      "TG",
      "TH",
      "TJ",
      "TK",
      "TL",
      "TM",
      "TN",
      "TO",
      "TP",
      "TR",
      "TT",
      "TV",
      "TW",
      "TZ",
      "UA",
      "UG",
      "UM",
      "UN",
      "US",
      "UY",
      "UZ",
      "VA",
      "VC",
      "VE",
      "VG",
      "VI",
      "VN",
      "VU",
      "WF",
      "WS",
      "YE",
      "YT",
      "ZA",
      "ZM",
      "ZW"
    )
}

object PrimitiveGen extends PrimitiveGen
