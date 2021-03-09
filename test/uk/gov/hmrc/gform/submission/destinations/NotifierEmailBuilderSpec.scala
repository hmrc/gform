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

package uk.gov.hmrc.gform.submission.destinations

import cats.implicits._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.notifier.{ NotifierEmail, NotifierEmailReference }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.{ Possible, Spec }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, PrimitiveGen, SubmissionRefGen }
import uk.gov.hmrc.gform.sharedmodel.notifier.{ NotifierEmailAddress, NotifierPersonalisationFieldId }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }

class NotifierEmailBuilderSpec
    extends Spec with DestinationGen with SubmissionRefGen with PrimitiveGen with ScalaCheckDrivenPropertyChecks {
  "apply" must "fail if the 'to' field does not exist in the model" in {
    forAll(emailGen.map(_.copy(personalisation = Map.empty))) { destination =>
      NotifierEmailBuilder[Possible](
        destination,
        StructuredFormValue.ObjectStructure(Nil)
      ) shouldBe Left(NotifierEmailBuilder.missingField(destination.id, destination.to))
    }
  }

  it must "succeed if the 'to' field exists in the model when there are no personalisation fields specified" in {
    forAll(emailGen.map(_.copy(personalisation = Map.empty)), nonEmptyAlphaNumStrGen) { (destination, to) =>
      val structuredFormData =
        StructuredFormValue.ObjectStructure(
          List(Field(FieldName(destination.to.value), StructuredFormValue.TextNode(to)))
        )

      val expectedNotifierEmail =
        NotifierEmail(
          destination.emailTemplateId,
          NotifierEmailAddress(to),
          Map.empty,
          NotifierEmailReference("")
        )

      NotifierEmailBuilder[Possible](
        destination,
        structuredFormData
      ) shouldBe Right(expectedNotifierEmail)
    }
  }

  it must "fail when the form data is missing personalisation fields" in {
    forAll(emailGen, nonEmptyAlphaNumStrGen, nonEmptyAlphaNumStrGen) { (generatedDestination, pf, to) =>
      val destination = generatedDestination.copy(
        personalisation = Map(NotifierPersonalisationFieldId(pf) -> FormComponentId(s"_$pf"))
      )
      val structuredFormData =
        StructuredFormValue.ObjectStructure(
          List(Field(FieldName(destination.to.value), StructuredFormValue.TextNode(to)))
        )

      NotifierEmailBuilder[Possible](
        destination,
        structuredFormData
      ) shouldBe Left(NotifierEmailBuilder.missingField(destination.id, FormComponentId(s"_$pf")))
    }
  }

  it must "succeed when all required form fields are present" in {
    forAll(emailGen, nonEmptyAlphaNumStrGen, nonEmptyAlphaNumStrGen) { (generatedDestination, pf, to) =>
      val destination = generatedDestination.copy(
        personalisation = Map(NotifierPersonalisationFieldId(pf) -> FormComponentId(s"_$pf"))
      )
      val structuredFormData =
        StructuredFormValue.ObjectStructure(
          List(
            Field(FieldName(destination.to.value), StructuredFormValue.TextNode(to)),
            Field(FieldName(s"_$pf"), StructuredFormValue.TextNode("some value"))
          )
        )

      val expectedNotifierEmail =
        NotifierEmail(
          destination.emailTemplateId,
          NotifierEmailAddress(to),
          Map(pf -> "some value"),
          NotifierEmailReference("")
        )

      NotifierEmailBuilder[Possible](
        destination,
        structuredFormData
      ) shouldBe Right(expectedNotifierEmail)
    }
  }
}
