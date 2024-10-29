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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TextExpression
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.HandlebarValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen

class UploadableDestinationSpec extends Spec with ScalaCheckDrivenPropertyChecks {
  "UploadableHandlebarsHttpApiDestination.toHandlebarsHttpApiDestination" should "not condition the uri, payload and includeIf if convertSingleQuotes is None" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      createUploadable(withQuotes).toHandlebarsHttpApiDestination shouldBe Right(
        withQuotes
      )
    }
  }

  it should "not condition the uri, payload and includeIf if convertSingleQuotes is Some(false)" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val destinationWithoutConvertSingleQuotes = destination.copy(convertSingleQuotes = Some(false))
      val withQuotes = addQuotes(destinationWithoutConvertSingleQuotes, """"'abc'"""")
      createUploadable(withQuotes).toHandlebarsHttpApiDestination shouldBe Right(withQuotes)
    }
  }

  it should "condition the uri, payload and includeIf if convertSingleQuotes is Some(true)" in {
    forAll(DestinationGen.handlebarsHttpApiGen) { destination =>
      val destinationWithConvertSingleQuotes = destination.copy(convertSingleQuotes = Some(true))
      val withQuotes = addQuotes(destinationWithConvertSingleQuotes, """'abc'""")
      val expected = withQuotes.copy(
        uri = replaceQuotes(withQuotes.uri),
        payload = withQuotes.payload.map(v => replaceQuotes(v)),
        includeIf = replaceHandlebarValue(withQuotes.includeIf)
      )

      createUploadable(withQuotes).toHandlebarsHttpApiDestination shouldBe Right(expected)
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
      val withQuotes = addQuotes(destination, """"'abc'"""").copy(convertSingleQuotes = Some(false))
      createUploadable(withQuotes, Some(false)).toHmrcDmsDestination shouldBe Right(withQuotes)
    }
  }

  it should "condition the includeIf if convertSingleQuotes is Some(true)" in {
    forAll(DestinationGen.hmrcDmsGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""").copy(convertSingleQuotes = Some(true))
      val expected = withQuotes.copy(
        includeIf = replaceHandlebarValue(withQuotes.includeIf)
      )

      createUploadable(withQuotes, Some(true)).toHmrcDmsDestination shouldBe Right(expected)
    }
  }

  "UploadableSubmissionConsolidator.toHmrctoSubmissionConsolidatorDestinationDmsDestination" should "not condition the includeIf,formData if convertSingleQuotes is None" in {
    forAll(DestinationGen.submissionConsolidatorGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      createUploadable(withQuotes, None).toSubmissionConsolidatorDestination shouldBe Right(withQuotes)
    }
  }

  it should "not condition the includeIf,formData if convertSingleQuotes is Some(false)" in {
    forAll(DestinationGen.submissionConsolidatorGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      createUploadable(withQuotes, Some(false)).toSubmissionConsolidatorDestination shouldBe Right(withQuotes)
    }
  }

  it should "condition the includeIf,formData if convertSingleQuotes is Some(true)" in {
    forAll(DestinationGen.submissionConsolidatorGen) { destination =>
      val withQuotes = addQuotes(destination, """"'abc'"""")
      val expected = withQuotes.copy(
        includeIf = replaceHandlebarValue(withQuotes.includeIf),
        formData = withQuotes.formData.map(replaceQuotes)
      )
      createUploadable(withQuotes, Some(true)).toSubmissionConsolidatorDestination shouldBe Right(expected)
    }
  }

  private def replaceHandlebarValue(includeIf: DestinationIncludeIf) =
    includeIf match {
      case HandlebarValue(s) => HandlebarValue(replaceQuotes(s))
      case _                 => HandlebarValue("")
    }

  private def createUploadable(
    destination: Destination.HandlebarsHttpApi
  ): UploadableHandlebarsHttpApiDestination = {
    import destination._
    UploadableHandlebarsHttpApiDestination(
      id,
      profile,
      uri,
      method,
      payload,
      Some(payloadType),
      convertSingleQuotes,
      includeIf,
      Some(failOnError),
      Some(multiRequestPayload)
    )
  }

  private def createUploadable(
    destination: Destination.HmrcDms,
    convertSingleQuotesParam: Option[Boolean]
  ): UploadableHmrcDmsDestination = {
    import destination._
    UploadableHmrcDmsDestination(
      id,
      dmsFormId,
      TextExpression(customerId),
      classificationType,
      businessArea,
      convertSingleQuotesParam,
      includeIf,
      Some(failOnError),
      Some(DataOutputFormat.XML),
      Some(formdataXml),
      backscan,
      instructionPdfFields
    )
  }

  private def createUploadable(
    destination: Destination.SubmissionConsolidator,
    convertSingleQuotes: Option[Boolean]
  ): UploadableSubmissionConsolidator = {
    import destination._
    UploadableSubmissionConsolidator(
      id,
      projectId,
      TextExpression(customerId),
      destination.formData,
      convertSingleQuotes,
      includeIf,
      Some(failOnError)
    )
  }

  private def replaceQuotes(s: String): String = SingleQuoteReplacementLexer(s).merge

  private def addQuotes(destination: Destination.HandlebarsHttpApi, q: String) =
    destination.copy(
      uri = q,
      payload = Some(q),
      includeIf = HandlebarValue(q)
    )

  private def addQuotes(destination: Destination.SubmissionConsolidator, q: String) =
    destination.copy(
      formData = Some(q),
      includeIf = HandlebarValue(q)
    )

  private def addQuotes(destination: Destination.HmrcDms, q: String) =
    destination.copy(includeIf = HandlebarValue(q))
}
