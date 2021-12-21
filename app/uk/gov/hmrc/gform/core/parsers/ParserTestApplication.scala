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

package uk.gov.hmrc.gform.core.parsers

import scala.util.parsing.input.CharSequenceReader

object ParserTestApplication extends App {

  val x = new ValueParser.PackratReader(new CharSequenceReader("${1 * 2 + 3}"))

  val res = ValueParser.parse(ValueParser.exprDeterminer, x)

  Console.println(res)

  val y = new ValueParser.PackratReader(
    new CharSequenceReader(
      "effectiveDate1 before TODAY || effectiveDate2 before TODAY || effectiveDate3 before TODAY || effectiveDate4 before TODAY || effectiveDate5 before TODAY}"
    )
  )

  val res2 = ValueParser.parse(ValueParser.booleanExpr, y)

  Console.println(res2.get)

}
