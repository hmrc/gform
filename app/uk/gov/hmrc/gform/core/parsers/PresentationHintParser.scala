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

package uk.gov.hmrc.gform.core.parsers

import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object PresentationHintParser {

  def validate(expression: String): Opt[List[PresentationHint]] = validateWithParser(expression, presentationHints)

  def validateSingle(expression: String): Opt[PresentationHint] = validateWithParser(expression, presentationHint)

  lazy val presentationHints: Parser[List[PresentationHint]] = (presentationHint ~ "," ~ presentationHints ^^ {
    (loc, presHint, _, presHints) =>
      presHint :: presHints
  }
    | presentationHint ^^ { (loc, presHint) =>
      List(presHint)
    })

  lazy val presentationHint: Parser[PresentationHint] = ("summariseGroupAsGrid" ^^ { (loc, unparsed) =>
    SummariseGroupAsGrid
  }
    | "invisibleInSummary" ^^ { (loc, unparsed) =>
      InvisibleInSummary
    }
    | "totalValue" ^^ { (loc, unparsed) =>
      TotalValue
    }
    | "invisiblePageTitleInSummary" ^^ { (loc, unparsed) =>
      InvisiblePageTitleInSummary
    })
}
