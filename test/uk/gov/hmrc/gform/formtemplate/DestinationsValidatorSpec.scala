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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import cats.syntax.eq._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.{ Invalid, Valid }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, Equals, FormComponentId, FormCtx, IncludeIf }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.{ HandlebarValue, IncludeIfValue }

class DestinationsValidatorSpec extends Spec with ScalaCheckDrivenPropertyChecks {
  "validateUniqueDestinationIds" should "return an error when there are duplicate ids" in {
    forAll(destinationIdGen, destinationIdGen) { (id1, id2) =>
      whenever(id1 =!= id2) {
        forAll(
          oneOrMoreGen(destinationWithFixedIdGen(id1)),
          oneOrMoreGen(destinationWithFixedIdGen(id1)),
          oneOrMoreGen(destinationWithFixedIdGen(id2)),
          oneOrMoreGen(destinationWithFixedIdGen(id2)),
          destinationGen.filter(d => d.id =!= id1 && d.id =!= id2)
        ) { (d1WithId1, d2WithId1, d1WithId2, d2WithId2, uniqueD) =>
          whenever(uniqueD.id =!= id1 && uniqueD.id =!= id2) {
            val destinations =
              Destinations
                .DestinationList(
                  uniqueD :: d1WithId1 ::: d2WithId1 ::: d1WithId2 ::: d2WithId2,
                  ackSection,
                  Some(decSection)
                )

            DestinationsValidator.validateUniqueDestinationIds(destinations) should be(
              Invalid(DestinationsValidator.someDestinationIdsAreUsedMoreThanOnce(Set(id1, id2)))
            )
          }
        }
      }
    }
  }

  it should "not return an error when there are no duplicate ids" in {
    forAll(destinationGen, destinationGen) { (d1, d2) =>
      val d1Ids = DestinationsValidator.extractIds(d1).toList
      val d2Ids = DestinationsValidator.extractIds(d2).toList

      val d1IdsSet = d1Ids.toSet
      val d2IdsSet = d2Ids.toSet

      whenever(d1Ids.size == d1IdsSet.size && d2Ids.size == d2IdsSet.size && d1IdsSet.intersect(d2IdsSet).isEmpty) {
        DestinationsValidator.validateUniqueDestinationIds(
          Destinations.DestinationList(NonEmptyList.of(d1, d2), ackSection, Some(decSection))
        ) should be(Valid)
      }
    }
  }

  "validateNoGroupInDeclaration" should "return an error when there is a Group component in Declaration section" in {
    val groupComponentId = DestinationsValidator.extractGroupComponentId(
      destinationListWithGroupComponentInDecSection.declarationSection.toList.flatMap(_.fields)
    )

    DestinationsValidator.validateNoGroupInDeclaration(destinationListWithGroupComponentInDecSection) should be(
      groupComponentId match {
        case Some(id) => Invalid(DestinationsValidator.groupComponentInDeclaration(id))
        case None     => Valid
      }
    )
  }

  it should "pass validation when there is not a Group component in Declaration section" in {
    DestinationsValidator.validateNoGroupInDeclaration(destinationList) should be(Valid)
  }

  "validateDestinationIncludeIfs" should "return an error when destinations have the different type of includeIf statements" in {
    val destinations = NonEmptyList.of(
      hmrcDms.copy(includeIf = IncludeIfValue(IncludeIf(Equals(FormCtx(FormComponentId("fieldA")), Constant("1"))))),
      hmrcDms.copy(includeIf = HandlebarValue("${empName = ''}"))
    )

    DestinationsValidator.validateDestinationIncludeIfs(
      destinationList.copy(destinations = destinations)
    ) shouldBe Invalid(
      "IncludeIf statements in hmrcDms destinations are not valid. Destinations 'includeIf' must be either all 'gform expressions' ie. ${...} or all 'handlebar expressions' ie. {{...}}. It cannot be mix of both."
    )
  }

  "validateDestinationIncludeIfs" should "pass validation when both are includeIf statement" in {
    val destinations = NonEmptyList.of(
      hmrcDms.copy(includeIf = IncludeIfValue(IncludeIf(Equals(FormCtx(FormComponentId("fieldA")), Constant("1"))))),
      hmrcDms.copy(includeIf = IncludeIfValue(IncludeIf(Equals(FormCtx(FormComponentId("fieldA")), Constant("2")))))
    )

    DestinationsValidator.validateDestinationIncludeIfs(destinationList.copy(destinations = destinations)) should be(
      Valid
    )
  }

  "validateDestinationIncludeIfs" should "pass validation when both are handlebar statement" in {
    val destinations = NonEmptyList.of(
      hmrcDms.copy(includeIf = HandlebarValue("{{isNotNull empName}}")),
      hmrcDms.copy(includeIf = HandlebarValue("{{isNull empName}}"))
    )

    DestinationsValidator.validateDestinationIncludeIfs(destinationList.copy(destinations = destinations)) should be(
      Valid
    )
  }

  "validateDestinationIncludeIfs" should "pass validation when both are handlebar statement in the same destination type" in {
    val destinations = NonEmptyList.of(
      hmrcDms.copy(includeIf = HandlebarValue("{{isNotNull empName}}")),
      hmrcDms.copy(includeIf = HandlebarValue("{{isNull empName}}")),
      dataStore.copy(includeIf = IncludeIfValue(IncludeIf(Equals(FormCtx(FormComponentId("fieldA")), Constant("1")))))
    )

    DestinationsValidator.validateDestinationIncludeIfs(destinationList.copy(destinations = destinations)) should be(
      Valid
    )
  }
}
