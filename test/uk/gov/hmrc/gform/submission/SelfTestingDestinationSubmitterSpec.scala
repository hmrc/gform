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

package uk.gov.hmrc.gform.submission

import cats.instances.either._
import org.scalacheck.Gen
import play.api.libs.json.JsString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen }
import uk.gov.hmrc.http.HeaderCarrier

class SelfTestingDestinationSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private val emptyModel = HandlebarsTemplateProcessorModel.empty
  type Possible[T] = Either[String, T]

  private def verifyError(
    actual: Possible[Option[HandlebarsDestinationResponse]],
    expected: Possible[Option[HandlebarsDestinationResponse]]) = {
    actual shouldBe expected
    actual shouldBe a[Left[_, _]]
  }

  "submitToDMS" should "always succeed" in {
    forAll(DestinationsGen.deprecatedDmsSubmissionGen) { destination =>
      val submitter = new SelfTestingDestinationSubmitter[Possible](test = DestinationTest("", emptyModel, Nil))
      submitter.submitToDms(null, destination) should be(Right(()))
    }
  }

  "submitIfIncludeIf" should "fail if no destination is specified within the test and if the destination includeIf evaluates to true" in {
    val submitter =
      new SelfTestingDestinationSubmitter[Possible](test = DestinationTest("", emptyModel, Nil))
    forAll(includedDestinationGen) { destination =>
      verifyError(
        submitter.submitIfIncludeIf(destination, null, emptyModel, null, null),
        submitter.includeIEvaluatedToTrueButNoTestDestinationInformationWasProvided(destination)
      )
    }
  }

  it should "pass if no destination is specified within the test and if the destination includeIf evaluates to false" in {
    val submitter =
      new SelfTestingDestinationSubmitter[Possible](test = DestinationTest("", emptyModel, Nil))
    forAll(includeIfDestinationGen(false)) { destination =>
      submitter.submitIfIncludeIf(destination, null, emptyModel, null, null) shouldBe a[Right[_, _]]
    }
  }

  it should "fail if the test includeIf is different from the destination's includeIf" in {
    val table = Table(
      ("testIncludeIf", "destinationIncludeIf"),
      (false, true),
      (true, false)
    )
    forAll(table) {
      case (testIncludeIf, destIncludeIf) =>
        forAll(includeIfDestinationGen(destIncludeIf)) { destination =>
          val destinationTestResult = DestinationTestResult(destination.id, testIncludeIf, None, None, None)
          val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

          val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
          verifyError(
            submitter.submitIfIncludeIf(destination, null, emptyModel, null, null),
            submitter.inconsistentIncludeIfs(destination, destIncludeIf, testIncludeIf)
          )
        }
    }
  }

  it should "pass if both destination includeIf and test includeIf are false" in {
    verifyWhenIncludeIfsAreTheSame(false)
  }

  it should "pass if both destination includeIf and test includeIf are true" in {
    verifyWhenIncludeIfsAreTheSame(true)
  }

  private def verifyWhenIncludeIfsAreTheSame(includeIf: Boolean) =
    forAll(includeIfDestinationGen(includeIf)) { destination =>
      val destinationTestResult =
        DestinationTestResult(
          destination.id,
          includeIf = includeIf,
          None,
          None,
          Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      submitter.submitIfIncludeIf(destination, null, emptyModel, null, null) shouldBe a[Right[_, _]]
    }

  it should "fail if the payloads are invalid json" in {
    val payload = "something"
    forAll(
      includedHandlebarsDestinationGen
        .map(setPayload(_, Some(payload)))) { destination =>
      val destinationTestResult =
        DestinationTestResult(
          destination.id,
          includeIf = true,
          None,
          Some(JsString(payload)),
          Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      val result = submitter.submitIfIncludeIf(destination, null, emptyModel, null, null)
      result should be(submitter.parseTransformedPayload(destination.id, payload))
      result shouldBe a[Left[_, _]]
      result.left.value should include("payload is not valid JSON")
    }
  }

  it should "fail if the payload does not match the expected payload" in {
    val actualPayload = """"something""""
    val expectedPayload = """"something else""""

    forAll(
      includedHandlebarsDestinationGen
        .map(setPayload(_, Some(actualPayload)))) { destination =>
      val destinationTestResult =
        DestinationTestResult(
          destination.id,
          includeIf = true,
          None,
          Some(JsString(expectedPayload)),
          Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      val result = submitter.submitIfIncludeIf(destination, null, emptyModel, null, null)
      verifyError(result, submitter.generatedPayloadDoesNotMatchExpected(destination))
    }
  }

  it should "fail if the payload does not exist yet the expected payload does" in {
    val expectedPayload = """"something else""""

    forAll(
      includedHandlebarsDestinationGen
        .map(setPayload(_, None))) { destination =>
      val destinationTestResult =
        DestinationTestResult(
          destination.id,
          includeIf = true,
          None,
          Some(JsString(expectedPayload)),
          Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      val result = submitter.submitIfIncludeIf(destination, null, emptyModel, null, null)
      verifyError(result, submitter.destinationHasNoPayloadButTestDoes(destination))
    }
  }

  it should "fail if the expected payload does not exist yet the actual payload does" in {
    val actualPayload = """"something""""

    forAll(
      includedHandlebarsDestinationGen
        .map(setPayload(_, Some(actualPayload)))) { destination =>
      val destinationTestResult =
        DestinationTestResult(destination.id, includeIf = true, None, None, Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      val result = submitter.submitIfIncludeIf(destination, null, emptyModel, null, null)
      verifyError(result, submitter.testHasNoPayloadButDestinationDoes(destination))
    }
  }

  it should "pass if the payload does match the expected payload" in {
    val actualPayload = """"something else""""
    val expectedPayload = """something else"""

    forAll(
      includedHandlebarsDestinationGen
        .map(setPayload(_, Some(actualPayload)))) { destination =>
      val destinationTestResult =
        DestinationTestResult(
          destination.id,
          includeIf = true,
          None,
          Some(JsString(expectedPayload)),
          Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      submitter.submitIfIncludeIf(destination, null, emptyModel, null, null) shouldBe a[Right[_, _]]
    }
  }

  it should "fail if no response code has been specified in the destination" in {
    forAll(includedHandlebarsDestinationGen) { destination =>
      val destinationTestResult =
        DestinationTestResult(destination.id, includeIf = true, None, None, None)
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))

      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      val result = submitter.submitIfIncludeIf(destination, null, emptyModel, null, null)
      verifyError(result, submitter.noResponseSpecified(destination.id))
    }
  }

  it should "fail if the destination has a templated URI and the test does not specify a URI" in {
    forAll(
      includedHandlebarsDestinationGen
        .map(setUri(_, "www.abc.com/{{foo}}"))) { destination =>
      val destinationTestResult =
        DestinationTestResult(destination.id, includeIf = true, None, None, Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", emptyModel, List(destinationTestResult))
      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      verifyError(
        submitter.submitIfIncludeIf(destination, null, emptyModel, null, null),
        submitter.noExpectedUriSpecified(destination))
    }
  }

  it should "fail if the destination has a templated URI and the test URI does not match the result of processing the templated URI" in {
    val model = HandlebarsTemplateProcessorModel("""{ "foo" : "bar" }""")
    forAll(
      includedHandlebarsDestinationGen
        .map(setUri(_, "www.abc.com/{{foo}}"))) { destination =>
      val destinationTestResult = DestinationTestResult(
        destination.id,
        includeIf = true,
        Some("www.abc.com/"),
        None,
        Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", model, List(destinationTestResult))
      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      verifyError(
        submitter.submitIfIncludeIf(destination, null, model, null, null),
        submitter.mismatchedUri(destination, "www.abc.com/", "www.abc.com/bar"))
    }
  }

  it should "pass if the destination has a templated URI and the test URI match the result of processing the templated URI" in {
    val model = HandlebarsTemplateProcessorModel("""{ "foo" : "bar" }""")
    forAll(
      includedHandlebarsDestinationGen
        .map(setUri(_, "www.abc.com/{{foo}}"))) { destination =>
      val destinationTestResult = DestinationTestResult(
        destination.id,
        includeIf = true,
        Some("www.abc.com/bar"),
        None,
        Some(DestinationTestResponse(200, None)))
      val test = DestinationTest("submitIfIncludeIfTest", model, List(destinationTestResult))
      val submitter = new SelfTestingDestinationSubmitter[Possible](test = test)
      submitter.submitIfIncludeIf(destination, null, model, null, null) shouldBe a[Right[_, _]]
    }
  }

  private def setUri(destination: Destination.HandlebarsHttpApi, uri: String): Destination.HandlebarsHttpApi =
    destination.copy(uri = uri)

  private def setIncludeIf(destination: Destination, includeIf: Option[String]): Destination = destination match {
    case d: Destination.HmrcDms           => d.copy(includeIf = includeIf.getOrElse(true.toString))
    case d: Destination.HandlebarsHttpApi => d.copy(includeIf = includeIf.getOrElse(true.toString))
    case d: Destination.Composite         => d.copy(includeIf = includeIf.getOrElse(true.toString))
    case d: Destination.StateTransition   => d.copy(includeIf = includeIf.getOrElse(true.toString))
    case d: Destination.ReviewingOfsted   => d.copy(includeIf = includeIf.getOrElse(true.toString))
    case d: Destination.ReviewRejection   => d.copy(includeIf = includeIf.getOrElse(true.toString))
    case d: Destination.ReviewApproval    => d.copy(includeIf = includeIf.getOrElse(true.toString))
  }

  private def setPayload(destination: Destination, payload: Option[String]): Destination = destination match {
    case d: Destination.HandlebarsHttpApi => d.copy(payload = payload)
    case d: Destination.Composite         => d
    case d: Destination.StateTransition   => d
    case d: Destination.HmrcDms           => d
    case d: Destination.ReviewingOfsted   => d
    case d: Destination.ReviewRejection   => d
    case d: Destination.ReviewApproval    => d
  }

  private def includedHandlebarsDestinationGen: Gen[Destination.HandlebarsHttpApi] =
    DestinationGen.handlebarsHttpApiGen
      .map(_.copy(includeIf = "true", payload = None))

  private def includedDestinationGen: Gen[Destination] = includeIfDestinationGen(true)

  private def includeIfDestinationGen(includeIf: Boolean): Gen[Destination] =
    DestinationGen.singularDestinationGen
      .map(setIncludeIf(_, Some(includeIf.toString)))
      .map(setPayload(_, None))
}
