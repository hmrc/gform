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

class UploadableDestinationSpec extends Spec {
  "UploadableHandlebarsHttpApiDestination.toHandlebarsHttpApiDestination" should "not condition the uri, payload and includeIf if convertSingleQuotes is None" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      createUploadable(withQuotes, None).toHandlebarsHttpApiDestination shouldBe Right(withQuotes)
    }
  }

  it should "not condition the uri, payload and includeIf if convertSingleQuotes is Some(false)" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      createUploadable(withQuotes, Some(false)).toHandlebarsHttpApiDestination shouldBe Right(withQuotes)
    }
  }

  it should "condition the uri, payload and includeIf if convertSingleQuotes is Some(true)" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val withQuotes = addQuotes(destination, """'abc'""")
      val expected = withQuotes.copy(
        uri = replaceQuotes(withQuotes.uri),
        payload = withQuotes.payload.map(v => replaceQuotes(v)),
        includeIf = replaceQuotes(withQuotes.includeIf)
      )

      createUploadable(withQuotes, Some(true)).toHandlebarsHttpApiDestination shouldBe Right(expected)
    }
  }

  "UploadableHmrcDmsDestination.toHmrcDmsDestination" should "not condition the includeIf if convertSingleQuotes is None" in {
    forAll(DestinationGen.hmrcDmsGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      createUploadable(withQuotes, None).toHmrcDmsDestination shouldBe Right(withQuotes)
    }
  }

  it should "not condition the includeIf if convertSingleQuotes is Some(false)" in {
    forAll(DestinationGen.hmrcDmsGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      createUploadable(withQuotes, Some(false)).toHmrcDmsDestination shouldBe Right(withQuotes)
    }
  }

  it should "condition the includeIf if convertSingleQuotes is Some(true)" in {
    forAll(DestinationGen.hmrcDmsGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      val expected = withQuotes.copy(
        includeIf = replaceQuotes(withQuotes.includeIf)
      )

      createUploadable(withQuotes, Some(true)).toHmrcDmsDestination shouldBe Right(expected)
    }
  }

  private def createUploadable(
    destination: Destination.HandlebarsHttpApi,
    convertSingleQuotes: Option[Boolean]): UploadableHandlebarsHttpApiDestination = {
    import destination._
    UploadableHandlebarsHttpApiDestination(
      id,
      profile,
      uri,
      method,
      payload,
      Some(payloadType),
      convertSingleQuotes,
      Some(includeIf),
      Some(failOnError)
    )
  }

  private def createUploadable(
    destination: Destination.HmrcDms,
    convertSingleQuotes: Option[Boolean]): UploadableHmrcDmsDestination = {
    import destination._
    UploadableHmrcDmsDestination(
      id,
      dmsFormId,
      customerId,
      classificationType,
      businessArea,
      convertSingleQuotes,
      Some(includeIf),
      Some(failOnError)
    )
  }

  private def replaceQuotes(s: String): String = SingleQuoteReplacementLexer(s).merge

  private def addQuotes(destination: Destination.HandlebarsHttpApi, q: String) =
    destination.copy(
      uri = q,
      payload = Some(q),
      includeIf = q
    )

  private def addQuotes(destination: Destination.HmrcDms, q: String) =
    destination.copy(includeIf = q)
}
