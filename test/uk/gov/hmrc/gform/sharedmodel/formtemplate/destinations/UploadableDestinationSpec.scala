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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.verifyRead

class UploadableDestinationSpec extends Spec {
  "toHandlebarsHttpApiDestination" should "not condition the uri, payload and includeIf if convertSingleQuotes is None" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val withQuotes = addQuotes(destination)
      import withQuotes._
      UploadableHandlebarsHttpApiDestination(
        id,
        profile,
        uri,
        method,
        payload,
        None,
        includeIf,
        failOnError
      ).toHandlebarsHttpApiDestination shouldBe Right(withQuotes)
    }
  }

  it should "not condition the uri, payload and includeIf if convertSingleQuotes is Some(false)" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val withQuotes = addQuotes(destination)
      import withQuotes._
      UploadableHandlebarsHttpApiDestination(
        id,
        profile,
        uri,
        method,
        payload,
        Some(false),
        includeIf,
        failOnError
      ).toHandlebarsHttpApiDestination shouldBe Right(withQuotes)
    }
  }

  it should "condition the uri, payload and includeIf if convertSingleQuotes is Some(true)" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val withQuotes = addQuotes(destination)
      val expected = withQuotes.copy(
        uri = replaceQuotes(withQuotes.uri),
        payload = withQuotes.payload.map(v => replaceQuotes(v)),
        includeIf = withQuotes.includeIf.map(v => replaceQuotes(v))
      )

      import withQuotes._
      UploadableHandlebarsHttpApiDestination(
        id,
        profile,
        uri,
        method,
        payload,
        Some(true),
        includeIf,
        failOnError
      ).toHandlebarsHttpApiDestination shouldBe Right(expected)
    }
  }

  private def replaceQuotes(s: String): String = SingleQuoteReplacementLexer(s).merge

  private def addQuotes(destination: Destination.HandlebarsHttpApi) =
    destination.copy(
      uri = destination.uri + quotes,
      payload = destination.payload.map(_ + quotes),
      includeIf = destination.includeIf.map(_ + quotes)
    )

  val quotes = """'abc'^def 'ghi' jkl^\"' '\""""
}
