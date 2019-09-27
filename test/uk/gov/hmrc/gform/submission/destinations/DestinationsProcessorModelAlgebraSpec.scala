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

package uk.gov.hmrc.gform.submission.destinations

import cats.data.NonEmptyList
import com.softwaremill.quicklens._
import org.scalatest.Assertion
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.form.BundledFormTreeNode
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, ObligationDetail, ObligationDetailGen, ObligationDetails, RecalculatedTaxPeriodKey, RetrievedObligations, TaxResponse, TaxResponseGen, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ ComponentTypeGen, FormGen }
import uk.gov.hmrc.gform.submission.Tree

class DestinationsProcessorModelAlgebraSpec extends Spec {
  "Multiple HmrcTaxPeriod" must "be serialized into json" in {

    forAll(
      FormGen.formGen,
      TaxResponseGen.taxResponseGen,
      ComponentTypeGen.hmrcTaxPeriodGen,
      ObligationDetailGen.obligationDetailGen,
      ObligationDetailGen.obligationDetailGen
    ) { (form, taxResponse, hmrcTaxPeriod, obligationDetail1, obligationDetail2) =>
      val formComponentId1 = FormComponentId("fcId_1")
      val formComponentId2 = FormComponentId("fcId_2")
      val periodKey1 = "ABC"
      val periodKey2 = "DEF"

      val updObligationDetail1 = obligationDetail1.modify(_.periodKey).setTo(periodKey1)
      val updObligationDetail2 = obligationDetail2.modify(_.periodKey).setTo(periodKey2)

      val updTaxResponse: FormComponentId => ObligationDetail => TaxResponse = fcId =>
        od =>
          taxResponse
            .modify(_.id.recalculatedTaxPeriodKey)
            .setTo(RecalculatedTaxPeriodKey(fcId, hmrcTaxPeriod))
            .modify(_.obligation.obligations)
            .setTo(List(ObligationDetails(List(od))))

      val updTaxResponse1 = updTaxResponse(formComponentId1)(updObligationDetail1)
      val updTaxResponse2 = updTaxResponse(formComponentId2)(updObligationDetail2)

      val updForm =
        form
          .modify(_.thirdPartyData.obligations)
          .setTo(RetrievedObligations(NonEmptyList(updTaxResponse1, updTaxResponse2 :: Nil)))
          .modify(_.formData)
          .setTo(FormData(List(FormField(formComponentId1, periodKey1), FormField(formComponentId2, periodKey2))))

      val model = DestinationsProcessorModelAlgebra.createHmrcTaxPeriods(updForm)

      val expectedJson: String => ObligationDetail => String =
        pk => od => s"""|{
                        |  "periodKey": "$pk",
                        |  "periodFrom": "${formatDate(od.inboundCorrespondenceFromDate)}",
                        |  "periodTo": "${formatDate(od.inboundCorrespondenceToDate)}"
                        |}""".stripMargin

      val expectedJson1 = expectedJson(periodKey1)(updObligationDetail1)
      val expectedJson2 = expectedJson(periodKey2)(updObligationDetail2)

      val verify: FormComponentId => String => Assertion =
        fcId => expected => model.model.findPath(fcId.value) shouldBe HandlebarsTemplateProcessorModel(expected).model

      verify(formComponentId1)(expectedJson1)
      verify(formComponentId2)(expectedJson2)
    }
  }

  "FormTree" must "be serialized into json" in {
    def tree(prefix: String, templateId: String, children: Tree[BundledFormTreeNode]*) =
      Tree(
        BundledFormTreeNode(
          FormIdData.WithAccessCode(
            UserId(s"${prefix}UserId"),
            FormTemplateId(templateId),
            AccessCode(s"${prefix}SubmissionRef")
          )
        ),
        children: _*
      )

    val formTree =
      tree(
        "root",
        "rootTemplate",
        tree("child1", "childTemplate1"),
        tree("child2", "childTemplate1"),
        tree("child3", "childTemplate2"))

    DestinationsProcessorModelAlgebra.createBundledFormTree(formTree) shouldBe HandlebarsTemplateProcessorModel(
      """|{
         |  "formBundle" : {
         |    "tree" : {
         |      "formId" : "rootUserId-rootTemplate-rootSubmissionRef",
         |      "submissionRef" : "rootSubmissionRef",
         |      "childTemplate1" : [
         |        {
         |          "formId" : "child1UserId-childTemplate1-child1SubmissionRef",
         |          "submissionRef" : "child1SubmissionRef"
         |        },
         |        {
         |          "formId" : "child2UserId-childTemplate1-child2SubmissionRef",
         |          "submissionRef" : "child2SubmissionRef"
         |        }
         |      ],
         |      "childTemplate2" : [
         |        {
         |          "formId" : "child3UserId-childTemplate2-child3SubmissionRef",
         |          "submissionRef" : "child3SubmissionRef"
         |        }
         |      ]
         |    },
         |    "submissionRefs" : [
         |      "rootSubmissionRef",
         |      "child1SubmissionRef",
         |      "child2SubmissionRef",
         |      "child3SubmissionRef"
         |    ]
         |  }
         |}""".stripMargin)
  }
}
