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

package uk.gov.hmrc.gform.sharedmodel

import munit.{ FunSuite, Location }
import play.api.libs.json.{ JsError, JsSuccess, Json, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ComponentType, FormComponent, OverseasAddress }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OverseasAddress.Configurable._

class ComponentTypeSpec extends FunSuite {

  test("Overseas address with all available customization") {
    val componentType =
      toComponentType("""|{
                         |  "type": "overseasAddress",
                         |  "id": "colombiaAddress",
                         |  "label": "",
                         |  "line2Mandatory": "true",
                         |  "cityMandatory": "false",
                         |  "postcodeMandatory": "true",
                         |  "countryDisplayed": "false"
                         |}""")

    val expected = OverseasAddress(
      List(Mandatory.Line2, Mandatory.Postcode),
      List(Optional.City),
      true,
      None,
      false
    )

    assertEquals(componentType, expected)

  }

  test("Overseas address with all flags having no effectful value") {
    val componentType =
      toComponentType("""|{
                         |  "type": "overseasAddress",
                         |  "id": "colombiaAddress",
                         |  "label": "",
                         |  "line2Mandatory": "false",
                         |  "cityMandatory": "true",
                         |  "postcodeMandatory": "false"
                         |}""")

    val expected = OverseasAddress(Nil, Nil, true, None, true)

    assertEquals(componentType, expected)

  }

  test("Overseas address bare bone") {
    val componentType =
      toComponentType("""|{
                         |  "type": "overseasAddress",
                         |  "id": "colombiaAddress",
                         |  "label": ""
                         |}""")

    val expected = OverseasAddress(Nil, Nil, true, None, true)

    assertEquals(componentType, expected)

  }

  test("Overseas address with countryLookup") {
    val componentType =
      toComponentType("""|{
                         |  "type": "overseasAddress",
                         |  "id": "colombiaAddress",
                         |  "label": "",
                         |  "countryLookup": "false"
                         |}""")

    val expected = OverseasAddress(Nil, Nil, false, None, true)

    assertEquals(componentType, expected)

  }

  test("Overseas address with countryDisplayed") {
    val componentType =
      toComponentType("""|{
           |  "type": "overseasAddress",
           |  "id": "colombiaAddress",
           |  "label": "",
           |  "countryLookup": "false",
           |  "countryDisplayed": "false"
           |}""")

    val expected = OverseasAddress(Nil, Nil, false, None, false)

    assertEquals(componentType, expected)

  }

  private def toComponentType(component: String)(implicit loc: Location): ComponentType = {

    val componentJson = Json.parse(component.stripMargin)

    implicitly[Reads[FormComponent]].reads(componentJson) match {
      case JsSuccess(formComponent, _) => formComponent.`type`
      case JsError(error)              => fail("Can't construct FormComponent instance", clues(error))
    }
  }
}
