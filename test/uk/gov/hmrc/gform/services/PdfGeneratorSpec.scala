/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.services

import org.scalatest._
import uk.gov.hmrc.gform.models._

class PdfGeneratorSpec extends FlatSpec with Matchers {

  def getFF(id: FieldId, label: String, value: String) = {

    val ff = FormField(id, value)
    val fv = FieldValue(
      id = id,
      `type` = Text(Constant(""), total = false),
      label = label,
      helpText = None,
      editable = true,
      mandatory = false,
      submissible = true
    )
    (ff, fv)
  }

  "PdfGenerator" should "render pdf" in {

    val sections = List(
      SectionFormField(
        "About you",
        List(
          getFF(FieldId("firstName"), "First Name", "Joe"),
          getFF(FieldId("surname"), "Surname", "Doe")
        )
      ),
      SectionFormField(
        "Business details",
        List(
          getFF(FieldId("businessName"), "Business Name", "HMRC"),
          getFF(FieldId("businessArea"), "Business Area", "Worthing"),
          getFF(FieldId("businessNino"), "Business Nino", "BA1234556")
        )
      ),
      SectionFormField(
        "Important dates",
        List(
          getFF(FieldId("year"), "Current year", "2017"),
          getFF(FieldId("nextChristmas"), "Next christmas", "Monday, December 25, 2017"),
          getFF(FieldId("showcase"), "Bforms showcase", "12/18/2017"),
          getFF(FieldId("summnerSolctice"), "Summer solctice", "Wednesday, June 21, 2017")
        )
      )
    )

    // Convenient test for development, rather than real test
    //PdfGenerator.generate(sections, "Insurance Premium Tax Return")
  }

}
