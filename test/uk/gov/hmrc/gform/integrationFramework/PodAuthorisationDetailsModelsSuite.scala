/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.integrationFramework

import munit.FunSuite
import play.api.libs.json.{ JsError, JsSuccess, Json }

class PodAuthorisationDetailsModelsSuite extends FunSuite {

  private val fullResponse = Json.parse(
    """|{
       |  "generalDecisionDetails": { "emailConsent": true },
       |  "exporterDetails": {
       |    "eori": "GB523456789586000",
       |    "fullName": "Andrew Flintoff",
       |    "ukAddress": {
       |      "name": "Rose Cottage",
       |      "streetAndNumber": "10 Downing Street",
       |      "postCode": "SW1A 2AA",
       |      "city": "London",
       |      "countryCode": "GB"
       |    }
       |  },
       |  "yourDetails": {
       |    "fullName": "Freddie Flintoff",
       |    "addressLine1": "111 Old Trafford Street",
       |    "addressLine2": "Old Trafford",
       |    "postCode": "MN12 7LD",
       |    "city": "Manchester",
       |    "country": "GB",
       |    "telephoneNumber": "0161 528467",
       |    "emailAddress": "andrew@gmail.com"
       |  },
       |  "podFtaDetails": {
       |    "exporterActivity": "Trading",
       |    "version": 1,
       |    "lastPodAuthModifiedDate": "2026-10-05T22:28:58.303182443Z",
       |    "goodsItems": [
       |      { "hs4Code": "1900", "goodsDescription": "Soft Drinks" },
       |      { "hs4Code": "1905", "goodsDescription": "Biscuits" }
       |    ]
       |  }
       |}""".stripMargin
  )

  private val minimalResponse = Json.parse(
    """|{
       |  "generalDecisionDetails": { "emailConsent": false },
       |  "exporterDetails": {
       |    "eori": "GB123456789000",
       |    "fullName": "Acme Limited",
       |    "ukAddress": {
       |      "streetAndNumber": "1 High Street",
       |      "city": "Leeds"
       |    }
       |  },
       |  "yourDetails": {
       |    "fullName": "Jane Smith",
       |    "addressLine1": "2 Low Street",
       |    "addressLine2": "Headingley",
       |    "postCode": "LS6 1AA",
       |    "city": "Leeds",
       |    "country": "GB",
       |    "emailAddress": "jane@example.com"
       |  },
       |  "podFtaDetails": {
       |    "exporterActivity": "Production",
       |    "version": 9999,
       |    "lastPodAuthModifiedDate": "2026-11-01T10:00:00.000000000Z",
       |    "goodsItems": [
       |      { "hs4Code": "2202", "goodsDescription": "Soft Drinks" }
       |    ]
       |  }
       |}""".stripMargin
  )

  test("parse full response and flatten to one row per goods item, repeating non-goods fields") {
    val rows = fullResponse.as[PodAuthorisationDetailsResponse].transform

    val expectedFirstRow = FlattenedPodAuthorisationDetails(
      emailConsent = true,
      exporterEori = "GB523456789586000",
      exporterFullName = "Andrew Flintoff",
      exporterAddressName = Some("Rose Cottage"),
      exporterStreetAndNumber = "10 Downing Street",
      exporterPostCode = Some("SW1A 2AA"),
      exporterCity = "London",
      exporterCountryCode = Some("GB"),
      yourDetailsFullName = "Freddie Flintoff",
      yourDetailsAddressLine1 = "111 Old Trafford Street",
      yourDetailsAddressLine2 = "Old Trafford",
      yourDetailsPostCode = "MN12 7LD",
      yourDetailsCity = "Manchester",
      yourDetailsCountry = "GB",
      yourDetailsTelephoneNumber = Some("0161 528467"),
      yourDetailsEmailAddress = "andrew@gmail.com",
      exporterActivity = "Trading",
      version = 1,
      lastPodAuthModifiedDate = "2026-10-05T22:28:58.303182443Z",
      hs4Code = Some("1900"),
      goodsDescription = Some("Soft Drinks")
    )

    assertEquals(
      rows,
      List(
        expectedFirstRow,
        expectedFirstRow.copy(hs4Code = Some("1905"), goodsDescription = Some("Biscuits"))
      )
    )
  }

  test("parse response with optional fields missing") {
    val rows = minimalResponse.as[PodAuthorisationDetailsResponse].transform

    assertEquals(rows.size, 1)
    val row = rows.head
    assertEquals(row.exporterAddressName, None)
    assertEquals(row.exporterPostCode, None)
    assertEquals(row.exporterCountryCode, None)
    assertEquals(row.yourDetailsTelephoneNumber, None)
    assertEquals(row.emailConsent, false)
    assertEquals(row.exporterActivity, "Production")
    assertEquals(row.version, 9999)
    assertEquals(row.hs4Code, Some("2202"))
    assertEquals(row.goodsDescription, Some("Soft Drinks"))
  }

  test("flattened rows omit missing optional fields when written as JSON") {
    val json = Json.toJson(minimalResponse.as[PodAuthorisationDetailsResponse].transform)
    val row = json(0)

    assertEquals((row \ "exporterAddressName").toOption, None)
    assertEquals((row \ "yourDetailsTelephoneNumber").toOption, None)
    assertEquals((row \ "emailConsent").as[Boolean], false)
    assertEquals((row \ "version").as[Int], 9999)
    assertEquals((row \ "hs4Code").as[String], "2202")
  }

  test("produce a single row without goods fields when goodsItems is empty") {
    val response = fullResponse.as[PodAuthorisationDetailsResponse]
    val rows = response.copy(podFtaDetails = response.podFtaDetails.copy(goodsItems = Nil)).transform

    assertEquals(rows.size, 1)
    assertEquals(rows.head.hs4Code, None)
    assertEquals(rows.head.goodsDescription, None)
    assertEquals(rows.head.exporterEori, "GB523456789586000")
  }

  test("fail to parse a business failure response") {
    val businessFailure = Json.parse(
      """{"businessFailure":{"podReference":"POD123456789012","failureCode":"2","validationMessage":"Decision not current"}}"""
    )

    businessFailure.validate[PodAuthorisationDetailsResponse] match {
      case JsSuccess(value, _) => fail(s"Expected a parse failure but got $value")
      case JsError(_)          => ()
    }
  }
}
