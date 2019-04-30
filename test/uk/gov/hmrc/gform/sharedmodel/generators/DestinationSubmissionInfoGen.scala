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

package uk.gov.hmrc.gform.sharedmodel.generators

import org.scalacheck.Gen
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionData, Variables }
import uk.gov.hmrc.gform.sharedmodel.form.DestinationSubmissionInfo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.EmailParametersRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }

trait DestinationSubmissionInfoGen {
  def destinationSubmissionInfoGen: Gen[DestinationSubmissionInfo] =
    for {
      formId     <- FormGen.formIdGen
      customerId <- PrimitiveGen.nonEmptyAlphaNumStrGen
      pdfHtml    <- PrimitiveGen.nonEmptyAlphaNumStrGen
    } yield
      DestinationSubmissionInfo(
        formId,
        customerId,
        None,
        SubmissionData(
          pdfHtml,
          Variables(JsObject(Seq())),
          StructuredFormValue.ObjectStructure(List(Field(FieldName("foo"), StructuredFormValue.TextNode("fooValue")))),
          EmailParametersRecalculated(Map.empty)
        )
      )
}

object DestinationSubmissionInfoGen extends DestinationSubmissionInfoGen
