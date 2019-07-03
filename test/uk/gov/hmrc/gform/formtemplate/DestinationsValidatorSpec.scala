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

package uk.gov.hmrc.gform.formtemplate

import cats.syntax.eq._
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.{ Invalid, Valid }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators._
import DestinationGen._
import DestinationsGen._
class DestinationsValidatorSpec extends Spec {
  "validateUniqueDestinationIds" should "return an error when there are duplicate ids" in {
    forAll(destinationIdGen, destinationIdGen) { (id1, id2) =>
      forAll(
        oneOrMoreGen(destinationWithFixedIdGen(id1)),
        oneOrMoreGen(destinationWithFixedIdGen(id1)),
        oneOrMoreGen(destinationWithFixedIdGen(id2)),
        oneOrMoreGen(destinationWithFixedIdGen(id2)),
        destinationGen.filter(d => d.id != id1 && d.id != id2)
      ) { (d1WithId1, d2WithId1, d1WithId2, d2WithId2, uniqueD) =>
        val destinations =
          Destinations.DestinationList(uniqueD :: d1WithId1 ::: d2WithId1 ::: d1WithId2 ::: d2WithId2)

        DestinationsValidator.validateUniqueDestinationIds(destinations) should be(
          Invalid(DestinationsValidator.someDestinationIdsAreUsedMoreThanOnce(Set(id1, id2))))
      }
    }
  }

  it should "not return an error when there are no duplicate ids" in {
    forAll(destinationGen, destinationGen) { (d1, d2) =>
      whenever(d1.id != d2.id) {
        DestinationsValidator.validateUniqueDestinationIds(Destinations.DestinationList(NonEmptyList.of(d1, d2))) should be(
          Valid)
      }
    }
  }

  "validateTestDestinationIdsExist" should "return success when there are no tests" in {
    forAll(destinationGen) { destinations =>
      DestinationsValidator.validateTestDestinationIdsExist(
        Destinations.DestinationList(NonEmptyList.of(destinations)),
        Nil) should be(Valid)
    }
  }

  "validateTestDestinationIdsExist" should "return success when the formTemplate.destinations is a DmsSubmission" in {
    forAll(deprecatedDmsSubmissionGen) { destinations =>
      DestinationsValidator.validateTestDestinationIdsExist(destinations, Nil) should be(Valid)
    }
  }

  it should "return failure when a test has a destination id that does not exist in the destination list" in {
    forAll(destinationGen, DestinationGen.destinationIdGen) { (d1, testDestinationId) =>
      whenever(d1.id =!= testDestinationId) {
        val test =
          DestinationTest(
            "Foo",
            HandlebarsTemplateProcessorModel.empty,
            List(DestinationTestResult(testDestinationId, true, None, None, None)))
        val destinations = Destinations.DestinationList(NonEmptyList.of(d1))
        DestinationsValidator.validateTestDestinationIdsExist(destinations, List(test)) shouldBe
          Invalid(DestinationsValidator.destinationTestReferencesANonExistentDestination(Set(testDestinationId)))
      }
    }
  }

  "validateOneOrMoreCustomerIdDestinations" should "not return an error when there is one or more hmrcDms destinations" in {
    forAll(PrimitiveGen.oneOrMoreGen(hmrcDmsGen)) { destination =>
      val destinations = Destinations.DestinationList(destination)
      DestinationsValidator.validateOneOrMoreCustomerIdDestinations(destinations) should be(Valid)
    }
  }

  it should "not return an error when there is a simpleSubmissionReference destination" in {
    forAll(simpleSubmissionReferenceGen) { destination =>
      val destinations = Destinations.DestinationList(NonEmptyList.of(destination))
      DestinationsValidator.validateOneOrMoreCustomerIdDestinations(destinations) should be(Valid)
    }
  }

  it should "return an error when there are no CustomerId Destinations" in {
    forAll(handlebarsHttpApiGen) { d1 =>
      val destinations = Destinations.DestinationList(NonEmptyList.of(d1))
      DestinationsValidator.validateOneOrMoreCustomerIdDestinations(destinations) should be(
        Invalid(DestinationsValidator.oneOrMoreCustomerIdDestinationsRequired))
    }
  }

  it should "return success when all destinations ids in tests exist in the destination list" in {
    forAll(destinationGen) { d1 =>
      val test =
        DestinationTest(
          "Foo",
          HandlebarsTemplateProcessorModel.empty,
          List(DestinationTestResult(d1.id, true, None, None, None)))
      val destinations = Destinations.DestinationList(NonEmptyList.of(d1))
      DestinationsValidator.validateTestDestinationIdsExist(destinations, List(test)) shouldBe Valid
    }
  }
}
