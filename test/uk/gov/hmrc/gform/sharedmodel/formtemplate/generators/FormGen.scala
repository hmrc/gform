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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.{NotChecked, UserId}
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.form.generators.EnvelopeExpiryDateGen

trait FormGen {
  def formIdGen: Gen[FormId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(FormId(_))
  def envelopeIdGen: Gen[EnvelopeId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(EnvelopeId(_))
  def userIdGen: Gen[UserId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(UserId(_))
  def formFieldGen: Gen[FormField] =
    for {
      id    <- FormComponentGen.formComponentIdGen
      value <- Gen.alphaNumStr
    } yield FormField(id, value)
  def formDataGen: Gen[FormData] = PrimitiveGen.zeroOrMoreGen(formFieldGen).map(FormData(_))
  def formStatusGen: Gen[FormStatus] = Gen.oneOf(InProgress, Summary, Validated, Signed, Submitted)

  def formGen: Gen[Form] =
    for {
      formId         <- formIdGen
      envelopeId     <- envelopeIdGen
      userId         <- userIdGen
      formTemplateId <- FormTemplateGen.formTemplateIdGen
      formData       <- formDataGen
      status         <- formStatusGen
      visitIndex     <- VisitIndexGen.visitIndexGen
      expiryDate     <- Gen.option(EnvelopeExpiryDateGen.envelopeExpiryDateGen)

    } yield Form(formId, envelopeId, userId, formTemplateId, formData, status, visitIndex, expiryDate, NotChecked)
}

object FormGen extends FormGen
