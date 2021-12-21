/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import org.scalameter.api._
import org.scalameter.picklers.noPickler.instance
import uk.gov.hmrc.gform.core.parsers.ValueParser
import scala.util.parsing.input.CharSequenceReader

object BooleanExprParserBenchmark extends Bench.LocalTime {
  val expressionsList: List[String] = List(
    "${amountA=22 || amountB=33 || amountC=22 || amountD=33 || amountE=22}",
    "${amountA=22 && amountB=33 && amountC=22 && amountD=33 && amountE=22}",
    "${amountA<=22 || amountB<=33 || amountC<=22 || amountD<=33 || amountE<=22}",
    "${!amountA<=22 || !amountB<=33 || !amountC<=22 || !amountD<=33 || !amountE<=22}",
    "${amountA contains 22 && amountB contains 33 && amountC contains 22 && amountD contains 33 && amountE contains 22}",
    "${(amountA=22) && (amountB=33) && (amountC=22) && (amountD=33) && (amountE=22)}",
    "${(amountA=22 && amountB=33) || (amountC=66 && amountD=77) || (amountE=22 && amountF=33) || (amountG=66 && amountH=77) || (amountI=22 && amountJ=33)}",
    "${effectiveDate1 before TODAY || effectiveDate2 before TODAY || effectiveDate3 before TODAY || effectiveDate4 before TODAY || effectiveDate5 before TODAY}",
    "${(businessIOSS1 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS2 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS3 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') " +
      "|| (businessIOSS4 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS5 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$')}",
    "${amountA=22 || amountB=33 || amountC=22 || amountD=33 || amountE=22 || amountF=22 || amountG=33 || amountH=22 || amountI=33 || amountJ=22}",
    "${amountA=22 && amountB=33 && amountC=22 && amountD=33 && amountE=22 && amountF=33 && amountG=22 && amountH=33 && amountI=22 && amountJ=33}",
    "${amountA<=22 || amountB<=33 || amountC<=22 || amountD<=33 || amountE<=22 || amountF<=33 || amountG<=22 || amountH<=33 || amountI<=22 || amountJ<=33}",
    "${!amountA<=22 || !amountB<=33 || !amountC<=22 || !amountD<=33 || !amountE<=22 || !amountF<=33 || !amountG<=22 || !amountH<=33 || !amountI<=22 || !amountJ<=33}",
    "${amountA Contains 22 && amountB Contains 33 && amountC Contains 22 && amountD Contains 33 && amountE Contains 22 && amountF Contains 33 && amountG Contains 22 && amountH Contains 33 && amountI Contains 22 && amountJ Contains 33}",
    "${(amountA=22) && (amountB=33) && (amountC=22) && (amountD=33) && (amountE=22) && (amountF=33) && (amountG=22) && (amountH=33) && (amountI=22) && (amountJ=33)}",
    "${(amountA=22 && amountB=33) || (amountC=66 && amountD=77) || (amountE=22 && amountF=33) || (amountG=66 && amountH=77) || (amountI=22 && amountJ=33) || (amountK=66 && amountL=77) || (amountM=22 && amountN=33) || (amountO=66 && amountP=77) || (amountQ=22 && amountR=33) || (amountS=66 && amountT=77)}",
    "${effectiveDate1 before TODAY || effectiveDate2 before TODAY || effectiveDate3 before TODAY || effectiveDate4 before TODAY || effectiveDate5 before TODAY || effectiveDate6 before TODAY || effectiveDate7 before TODAY || effectiveDate8 before TODAY || effectiveDate9 before TODAY || effectiveDate10 before TODAY}",
    "${(businessIOSS1 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS2 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS3 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') " +
      "|| (businessIOSS4 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS5 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS6 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$')}" +
      "|| (businessIOSS7 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS8 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$') || (businessIOSS9 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$')}" +
      "|| (businessIOSS10 match '^[^a-zA-Z0-9]*[iI][mM][^a-zA-Z0-9]*[0-9]{10}[^a-zA-Z0-9]*$')}"
  )

  val expressions: Gen[String] = Gen.enumeration("Expression")(expressionsList: _*)

  performance of "BooleanExprParser" in {
    measure method "validate" in {
      using(expressions) in { expression =>
        val y = new ValueParser.PackratReader( new CharSequenceReader(expression))
        ValueParser.parse(ValueParser.booleanExpr, y).get
      }
    }
  }
}
