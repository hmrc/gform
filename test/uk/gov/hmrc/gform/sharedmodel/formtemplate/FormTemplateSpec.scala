/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json.{ JsError, JsSuccess }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen, FormTemplateGen }

class FormTemplateSpec extends Spec {
  "FormTemplate" should "round trip derived JSON" in {
    forAll(FormTemplateGen.formTemplateGen) { template =>
      FormTemplate.format.reads(FormTemplate.format.writes(template)) should beJsSuccess(template)
    }
  }

  it should "read JSON with a dmsSubmission field instead of a destinations field" in {
    forAll(FormTemplateGen.formTemplateGen, DestinationsGen.deprecatedDmsSubmissionGen) { (template, dmsSubmission) =>
      val dmsJson = Destinations.dmsSubmissionFormat.writes(dmsSubmission)
      val templateJson = FormTemplate.format.writes(template) - "destinations" + ("dmsSubmission" -> dmsJson)

      FormTemplate.format.reads(templateJson) match {
        case JsSuccess(value, _) =>
          value.destinations match {
            case legacy: Destinations.DmsSubmission => legacy should be(dmsSubmission)
            case other                              => fail(s"Got unexpected destinations: $other")
          }
        case JsError(errors) => fail(errors.toString)
      }
    }
  }

  it should "reject JSON with no dmsSubmission and no destinations fields" in {
    forAll(FormTemplateGen.formTemplateGen) { template =>
      val templateJson = FormTemplate.format.writes(template) - "destinations"

      FormTemplate.format.reads(templateJson) match {
        case e: JsError => e should be(FormTemplate.onlyOneOfDmsSubmissionAndDestinationsMustBeDefined)
        case _          => fail("Did not get an JsError and should have")
      }
    }
  }

  it should "reject JSON with both dmsSubmission and destinations fields" in {
    forAll(
      FormTemplateGen.formTemplateGen,
      DestinationsGen.destinationListGen,
      DestinationsGen.deprecatedDmsSubmissionGen) { (template, destinations, dmsSubmission) =>
      val dmsJson = Destinations.dmsSubmissionFormat.writes(dmsSubmission)
      val templateJson = FormTemplate.format.writes(template) + ("dmsSubmission" -> dmsJson)

      FormTemplate.format.reads(templateJson) match {
        case e: JsError => e should be(FormTemplate.onlyOneOfDmsSubmissionAndDestinationsMustBeDefined)
        case _          => fail("Did not get an JsError and should have")
      }
    }
  }

  it should "reject JSON with multiple HmrcDms destinations" in {
    forAll(FormTemplateGen.formTemplateGen, DestinationGen.hmrcDmsGen, DestinationGen.hmrcDmsGen) {
      (template, hmrcDms1, hmrcDms2) =>
        val dmsJson = Destinations.dmsSubmissionFormat.writes(dmsSubmission)
        val templateJson = FormTemplate.format.writes(template) + ("dmsSubmission" -> dmsJson)

        FormTemplate.format.reads(templateJson) match {
          case e: JsError => e should be(FormTemplate.onlyOneOfDmsSubmissionAndDestinationsMustBeDefined)
          case _          => fail("Did not get an JsError and should have")
        }
    }
  }
}
