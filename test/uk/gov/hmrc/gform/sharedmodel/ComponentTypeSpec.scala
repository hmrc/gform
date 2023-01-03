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
import uk.gov.hmrc.gform.Helpers._
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
                         |  "value": {
                         |    "line1": "Entrada 7 Via Cerritos",
                         |    "line2": "Condominio Samanes de Tacurumbi",
                         |    "line3": "Casa 2",
                         |    "city": "Pereira",
                         |    "postcode": "600007",
                         |    "country": "Colombia"
                         |  }
                         |}""")

    import OverseasAddress._
    val expected = OverseasAddress(
      List(Mandatory.Line2, Mandatory.Postcode),
      List(Optional.City),
      Some(
        Value(
          toSmartString("Entrada 7 Via Cerritos"),
          toSmartString("Condominio Samanes de Tacurumbi"),
          toSmartString("Casa 2"),
          toSmartString("Pereira"),
          toSmartString("600007"),
          toSmartString("Colombia")
        )
      ),
      true
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

    val expected = OverseasAddress(Nil, Nil, None, true)

    assertEquals(componentType, expected)

  }

  test("Overseas address bare bone") {
    val componentType =
      toComponentType("""|{
                         |  "type": "overseasAddress",
                         |  "id": "colombiaAddress",
                         |  "label": ""
                         |}""")

    val expected = OverseasAddress(Nil, Nil, None, true)

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

    val expected = OverseasAddress(Nil, Nil, None, false)

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
