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

package pact.uk.gov.hmrc.gform

import com.itv.scalapact.ScalaPactVerify.{ loadFromLocal, verifyPact }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AcknowledgementSection, Constant, DeclarationSection, FormTemplate, FormTemplateId, HmrcSimpleModule, TextExpression }

import scala.concurrent.ExecutionContext.Implicits.global

class GFormConnectorVerifiersPactSpec extends Spec with StubServer {

  ignore should "verify contract" in {
    verifyPact
      .withPactSource(loadFromLocal("delivered_pacts/gform-frontend_gform.json"))
      .setupProviderState("given") {
        case "Form 123 exists" =>
          persistAFormTemplate(FormTemplateId("222"))
          true
        case "Form 333 exists" =>
          persistAFormTemplate(FormTemplateId("333"))
          true
        case _ => false
      }
      .runVerificationAgainst("localhost", 9197)
  }

  def persistAFormTemplate(formTemplateId: FormTemplateId) = {
    val akn = AcknowledgementSection("Mr", None, None, Nil)
    val declaration = DeclarationSection("Mr", None, None, Nil)
    val submission = DmsSubmission("id", TextExpression(Constant("costant")), "classification", "BA")
    val raw = FormTemplate(
      formTemplateId,
      "name",
      "description",
      None,
      None,
      None,
      None,
      submission,
      None,
      HmrcSimpleModule,
      "classification",
      None,
      "",
      "business",
      None,
      Nil,
      akn,
      declaration,
      None
    )

    stubbedModule.module.formTemplateModule.formTemplateService.verifyAndSave(raw)
  }
}
