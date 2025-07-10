/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.it.companieshouse

import ch.qos.logback.classic.Level
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import play.api.http.Status.OK
import play.api.libs.json.{ JsValue, Json }
import play.api.test.Helpers.AUTHORIZATION
import play.api.test.{ DefaultAwaitTimeout, FutureAwaits }
import uk.gov.hmrc.gform.companieshouse.CompaniesHouseConnector
import uk.gov.hmrc.gform.it.{ ITSpec, LogCapturing }

class CompaniesHouseControllerISpec extends ITSpec with LogCapturing with DefaultAwaitTimeout with FutureAwaits {

  private def resource(resource: String): String = s"$baseUrl$resource"

  "GET company by number" should "return the same response from api to caller" in new Setup {
    stubFor(
      WireMock.get(urlEqualTo(s"/company/$companyNumber")).willReturn(aResponse().withStatus(200).withBody(companyBody))
    )
    val url = s"/companieshouse/company/$companyNumber"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 200
    (Json.parse(response.body) \ "company_number").as[String] should be(companyNumber)
  }

  it should "return 404 unknown company number" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber")).willReturn(aResponse().withStatus(404)))
    val url = s"/companieshouse/company/$companyNumber"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 404
  }

  it should "return 500 when companies house returns 429" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber")).willReturn(aResponse().withStatus(429)))
    val url = s"/companieshouse/company/$companyNumber"

    withCaptureOfLoggingFrom[CompaniesHouseConnector] { logs =>
      val response = await(wsClient.url(resource(url)).get())
      response.status shouldBe 500
      val errorLogs = logs.filter(_.getLevel == Level.ERROR).map(_.getMessage)
      errorLogs.head shouldBe "Received rate limit response from companies house"

    }
  }

  it should "not leak MDTP headers to companies house" in new Setup {
    // note: order matters as of wiremock 2.26
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber")).willReturn(ok(companyBody)))
    stubFor(
      WireMock
        .get(s"/company/$companyNumber")
        .withHeader(AUTHORIZATION, equalTo("Bearer authToken"))
        .willReturn(badRequest().withBody("leaked auth token"))
    )
    stubFor(
      WireMock
        .get(s"/company/$companyNumber")
        .withHeader(uk.gov.hmrc.http.HeaderNames.xSessionId, equalTo("some session id"))
        .willReturn(badRequest().withBody("leaked session id"))
    )

    val response = await(
      wsClient
        .url(resource(s"/companieshouse/company/$companyNumber"))
        .addHttpHeaders(AUTHORIZATION -> "Bearer authToken")
        .addHttpHeaders(uk.gov.hmrc.http.HeaderNames.xSessionId -> "some session id")
        .get()
    )
    response.status shouldBe OK
  }

  "GET company officers" should "return the same response (for officers) from api to caller" in new Setup {
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/officers"))
        .willReturn(aResponse().withStatus(200).withBody(officersBody.toString()))
    )
    val url = s"/companieshouse/company/$companyNumber/officers"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 200
    (Json.parse(response.body) \ "items").as[Seq[JsValue]].size should be(2)
  }

  it should "return the 500 (for officers) when api returns 429" in new Setup {
    withCaptureOfLoggingFrom[CompaniesHouseConnector] { logs =>
      stubFor(
        WireMock
          .get(urlEqualTo(s"/company/$companyNumber/officers"))
          .willReturn(aResponse().withStatus(429).withBody(officersBody.toString()))
      )
      val url = s"/companieshouse/company/$companyNumber/officers"
      val response = await(wsClient.url(resource(url)).get())
      response.status shouldBe 500
      (Json.parse(response.body) \ "error").as[String] shouldBe "Received rate limit response from companies house"
      val errorLogs = logs.filter(_.getLevel == Level.ERROR).map(_.getMessage)
      errorLogs.head shouldBe "Received rate limit response from companies house"
    }
  }

  it should "return a successful response for officers when a surname is requested with a filtered out resigned on user" in new Setup {
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/officers?items_per_page=100&start_index=100"))
        .willReturn(aResponse().withStatus(200).withBody(officersBody100InvalidResults.toString()))
    )
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/officers?items_per_page=100&start_index=0"))
        .willReturn(aResponse().withStatus(200).withBody(officersBody.toString()))
    )
    val url = s"/companieshouse/company/$companyNumber/officers?surname=ferguson"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 200
    (Json.parse(response.body) \ "items").as[Seq[JsValue]].size should be(1)
  }

  it should "return a successful response for officers when a surname is requested with 2 pages" in new Setup {
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/officers?items_per_page=100&start_index=0"))
        .willReturn(aResponse().withStatus(200).withBody(officersBody100InvalidResults.toString()))
    )
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/officers?items_per_page=100&start_index=100"))
        .willReturn(aResponse().withStatus(200).withBody(officersBody.toString()))
    )
    val url = s"/companieshouse/company/$companyNumber/officers?surname=ferguson"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 200
    (Json.parse(response.body) \ "items").as[Seq[JsValue]].size should be(1)
  }

  it should "return 404 if the response of officers does not contain the surname requested" in new Setup {
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/officers?items_per_page=100&start_index=0"))
        .willReturn(aResponse().withStatus(200).withBody(officersBody.toString()))
    )
    val url = s"/companieshouse/company/$companyNumber/officers?surname=invalid"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 404
  }

  it should "successfully parse the Capgemini officers search results" in new Setup {
    stubFor(WireMock.get(s"/company/$companyNumber/officers").willReturn(okJson(capgeminiOfficers.toString)))

    val url = s"/companieshouse/company/$companyNumber/officers"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe OK
  }

  it should "return 404 unknown company number for offices" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/officers")).willReturn(aResponse().withStatus(404)))
    val url = s"/companieshouse/company/$companyNumber/officers"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 404
  }

  it should "return 401 unauthorised if call to companiesHouse company returns 401" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber")).willReturn(aResponse().withStatus(401)))
    val url = s"/companieshouse/company/$companyNumber"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 401
  }

  it should "return 401 unauthorised if call to companiesHouse officers returns 401" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/officers")).willReturn(aResponse().withStatus(401)))
    val url = s"/companieshouse/company/$companyNumber/officers"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 401
  }

  it should "return 403 unauthorised if call to companiesHouse company returns 403" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber")).willReturn(aResponse().withStatus(403)))
    val url = s"/companieshouse/company/$companyNumber"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 403
  }

  it should "return 403 unauthorised if call to companiesHouse officers returns 403" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/officers")).willReturn(aResponse().withStatus(403)))
    val url = s"/companieshouse/company/$companyNumber/officers"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 403
  }

  it should "return 503 exception if service is unavailable" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/officers")).willReturn(aResponse().withFixedDelay(2000)))
    val url = s"/companieshouse/company/$companyNumber/officers"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 503
  }

  it should "not leak MDTP headers to companies house" in new Setup {
    // note: order matters as of wiremock 2.26
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/officers")).willReturn(ok(officersBody.toString)))
    stubFor(
      WireMock
        .get(s"/company/$companyNumber/officers")
        .withHeader(AUTHORIZATION, equalTo("Bearer authToken"))
        .willReturn(badRequest().withBody("leaked auth token"))
    )
    stubFor(
      WireMock
        .get(s"/company/$companyNumber/officers")
        .withHeader(uk.gov.hmrc.http.HeaderNames.xSessionId, equalTo("some session id"))
        .willReturn(badRequest().withBody("leaked session id"))
    )

    val response = await(
      wsClient
        .url(resource(s"/companieshouse/company/$companyNumber/officers"))
        .addHttpHeaders(AUTHORIZATION -> "Bearer authToken")
        .addHttpHeaders(uk.gov.hmrc.http.HeaderNames.xSessionId -> "some session id")
        .get()
    )
    response.status shouldBe OK
  }

  it should "[GG-8638] accept an item that hasn't got an address" in new Setup {
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/officers"))
        .willReturn(aResponse().withStatus(200).withBody(officersBodyNoAddress.toString()))
    )
    val url = s"/companieshouse/company/$companyNumber/officers"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 200
    (Json.parse(response.body) \ "items").as[Seq[JsValue]].size should be(1)
    (Json.parse(response.body) \ "items" \ 0 \ "address").asOpt[JsValue] shouldBe empty
  }

  "GET company insolvency information" should "return the same response from api to caller" in new Setup {
    stubFor(
      WireMock
        .get(urlEqualTo(s"/company/$companyNumber/insolvency"))
        .willReturn(aResponse().withStatus(200).withBody(insolvencyBody))
    )
    val url = s"/companieshouse/company/$companyNumber/insolvency"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 200
    (Json.parse(response.body) \ "cases").as[Seq[JsValue]].size should be(3)
    (Json.parse(response.body) \ "cases" \ 0 \ "type").as[String] shouldBe "receiver-manager"
    (Json.parse(response.body) \ "cases" \ 2 \ "type").as[String] shouldBe "in-administration"
  }

  it should "return 404 unknown company number" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/insolvency")).willReturn(aResponse().withStatus(404)))
    val url = s"/companieshouse/company/$companyNumber/insolvency"
    val response = await(wsClient.url(resource(url)).get())
    response.status shouldBe 404
  }

  it should "return 500 when companies house returns 429" in new Setup {
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/insolvency")).willReturn(aResponse().withStatus(429)))
    val url = s"/companieshouse/company/$companyNumber/insolvency"

    withCaptureOfLoggingFrom[CompaniesHouseConnector] { logs =>
      val response = await(wsClient.url(resource(url)).get())
      response.status shouldBe 500
      val errorLogs = logs.filter(_.getLevel == Level.ERROR).map(_.getMessage)
      errorLogs.head shouldBe "Received rate limit response from companies house"
    }
  }

  it should "not leak MDTP headers to companies house" in new Setup {
    // note: order matters as of wiremock 2.26
    stubFor(WireMock.get(urlEqualTo(s"/company/$companyNumber/insolvency")).willReturn(ok(insolvencyBody)))
    stubFor(
      WireMock
        .get(s"/company/$companyNumber/insolvency")
        .withHeader(AUTHORIZATION, equalTo("Bearer authToken"))
        .willReturn(badRequest().withBody("leaked auth token"))
    )
    stubFor(
      WireMock
        .get(s"/company/$companyNumber/insolvency")
        .withHeader(uk.gov.hmrc.http.HeaderNames.xSessionId, equalTo("some session id"))
        .willReturn(badRequest().withBody("leaked session id"))
    )

    val response = await(
      wsClient
        .url(resource(s"/companieshouse/company/$companyNumber/insolvency"))
        .addHttpHeaders(AUTHORIZATION -> "Bearer authToken")
        .addHttpHeaders(uk.gov.hmrc.http.HeaderNames.xSessionId -> "some session id")
        .get()
    )
    response.status shouldBe OK
  }

  private trait Setup {
    val companyNumber = "01234567"

    val companyBody =
      """
        |{
        | "accounts":{
        |   "accounting_reference_date":{
        |     "day":"integer","month":"integer"
        |   },
        |   "last_accounts":{
        |     "made_up_to":"date",
        |     "type":"string"
        |   },
        |   "next_due":"date",
        |   "next_made_up_to":"date",
        |   "overdue":"boolean"
        | },
        | "annual_return":{
        |   "last_made_up_to":"date",
        |   "next_due":"date",
        |   "next_made_up_to":"date",
        |   "overdue":"boolean"
        | },
        | "branch_company_details":{
        |   "business_activity":"string",
        |   "parent_company_name":"string",
        |   "parent_company_number":"string"
        | },
        | "can_file":"boolean",
        | "company_name":"string",
        | "company_number":"01234567",
        | "company_status":null,
        | "company_status_detail":"string",
        | "confirmation_statement":{
        |   "last_made_up_to":"date",
        |   "next_due":"date",
        |   "next_made_up_to":"date",
        |   "overdue":"boolean"
        | },
        | "date_of_creation":"date",
        | "date_of_dissolution":"date",
        | "etag":"string",
        | "foreign_company_details":{
        |   "accounting_requirement":{
        |     "foreign_account_type":"string",
        |     "terms_of_account_publication":"string"
        |   },
        |   "accounts":{
        |     "account_period_from":{
        |       "day":"integer",
        |       "month":"integer"
        |     },
        |     "account_period_to":{
        |       "day":"integer",
        |       "month":"integer"
        |     },
        |     "must_file_within":{
        |       "months":"integer"
        |     }
        |   },
        |   "business_activity":"string",
        |   "company_type":"string",
        |   "governed_by":"string",
        |   "is_a_credit_finance_institution":"boolean",
        |   "originating_registry":{
        |     "country":"string",
        |     "name":"string"
        |   },
        |   "registration_number":"string"
        | },
        | "has_been_liquidated":"boolean",
        | "has_charges":"boolean",
        | "has_insolvency_history":"boolean",
        | "is_community_interest_company":"boolean",
        | "jurisdiction":"string",
        | "last_full_members_list_date":"date",
        | "links":{
        |   "persons_with_significant_control_list":"string",
        |   "persons_with_significant_control_statements_list":"string",
        |   "self":"string"
        | },
        | "officer_summary":{
        |   "active_count":"integer",
        |   "officers":[{
        |     "appointed_on":"date",
        |     "date_of_birth":{
        |       "day":23,
        |       "month":4,
        |       "year":1948
        |     },
        |     "name":"Jim Ferguson",
        |     "officer_role":"director"
        |   }],
        |   "resigned_count":"integer"
        | },"registered_office_address":{
        |   "address_line_1":"string",
        |   "address_line_2":"string",
        |   "care_of":"string",
        |   "country":"string",
        |   "locality":"string",
        |   "po_box":"string",
        |   "postal_code":"string",
        |   "premises":"string",
        |   "region":"string"
        | },"registered_office_is_in_dispute":"boolean",
        | "sic_codes":["string"],
        | "type":"string",
        | "undeliverable_registered_office_address":"boolean"
        |}
      """.stripMargin

    val insolvencyBody =
      """
        |{
        |  "etag": "somelongstring",
        |  "cases": [
        |    {
        |      "type": "receiver-manager",
        |      "dates": [],
        |      "practitioners": [
        |        {
        |          "name": "John Smith",
        |          "address": {
        |            "address_line_1": "1 Somewhere St",
        |            "address_line_2": "Someburb",
        |            "locality": "Somewheresville",
        |            "postal_code": "SM1 2WH"
        |          },
        |          "appointed_on": "2022-08-05",
        |          "role": "receiver-manager"
        |        },
        |        {
        |          "name": "Jane Doe",
        |          "address": {
        |            "address_line_1": "1 Somewhere St",
        |            "address_line_2": "Someburb",
        |            "locality": "Somewheresville",
        |            "postal_code": "SM1 2WH"
        |          },
        |          "appointed_on": "2022-08-05",
        |          "role": "receiver-manager"
        |        }
        |      ],
        |      "links": {
        |        "charge": "/company/01234567/charges/somestring"
        |      },
        |      "number": "1"
        |    },
        |    {
        |      "type": "receiver-manager",
        |      "dates": [],
        |      "practitioners": [
        |        {
        |          "name": "John Smith",
        |          "address": {
        |            "address_line_1": "1 Somewhere St",
        |            "address_line_2": "Someburb",
        |            "locality": "Somewheresville",
        |            "postal_code": "SM1 2WH"
        |          },
        |          "appointed_on": "2022-08-05",
        |          "role": "receiver-manager"
        |        },
        |        {
        |          "name": "Jane Doe",
        |          "address": {
        |            "address_line_1": "1 Somewhere St",
        |            "address_line_2": "Someburb",
        |            "locality": "Somewheresville",
        |            "postal_code": "SM1 2WH"
        |          },
        |          "appointed_on": "2022-08-05",
        |          "role": "receiver-manager"
        |        }
        |      ],
        |      "links": {
        |        "charge": "/company/01234567/charges/somestring"
        |      },
        |      "number": "2"
        |    },
        |    {
        |      "type": "in-administration",
        |      "dates": [
        |        {
        |          "type": "administration-started-on",
        |          "date": "2022-10-21"
        |        }
        |      ],
        |      "practitioners": [
        |        {
        |          "name": "John McSmith",
        |          "address": {
        |            "address_line_1": "58 Somewhere Rd",
        |            "locality": "Somewheresville",
        |            "region": "Somewhereshire",
        |            "postal_code": "SM2 1WH"
        |          },
        |          "role": "practitioner"
        |        }
        |      ],
        |      "number": "3"
        |    }
        |  ],
        |  "status": [
        |    "in-administration",
        |    "receiver-manager"
        |  ]
        |}
        |""".stripMargin

    val resignedBody = Json.obj(
      "appointed_on"         -> "2015-04-10",
      "occupation"           -> "Director",
      "country_of_residence" -> "United Kingdom",
      "date_of_birth" -> Json.obj(
        "year"  -> 1948,
        "month" -> 4
      ),
      "officer_role" -> "director",
      "address" -> Json.obj(
        "locality"       -> "London",
        "country"        -> "England",
        "address_line_1" -> "9 Charcot Road",
        "premises"       -> "Flat 6, Osler Court",
        "postal_code"    -> "NW9 5XW"
      ),
      "name" -> "FERGUSON, Jim",
      "links" -> Json.obj(
        "officer" -> Json.obj(
          "appointments" -> "/officers/mlvDFJq0QpFX1hTw93U7MJNh_ko/appointments"
        )
      ),
      "nationality" -> "Italian",
      "resigned_on" -> "1999-01-01"
    )

    val officersBody100InvalidResults = Json.obj(
      "total_results"  -> 3,
      "items_per_page" -> 35,
      "etag"           -> "0905d15615b770cd4fcc27fdc1c959474ae4c03e",
      "items"          -> Json.toJson(List.fill(100)(resignedBody)),
      "links" -> Json.obj(
        "self" -> "/company/01234567/appointments"
      ),
      "active_count"   -> 2,
      "total_results"  -> 101,
      "kind"           -> "officer-list",
      "start_index"    -> 0,
      "resigned_count" -> 1
    )

    val officersBody = Json.obj(
      "total_results"  -> 3,
      "items_per_page" -> 35,
      "etag"           -> "0905d15615b770cd4fcc27fdc1c959474ae4c03e",
      "items" -> Json.arr(
        Json.obj(
          "appointed_on"         -> "2015-04-10",
          "occupation"           -> "Director",
          "country_of_residence" -> "United Kingdom",
          "date_of_birth" -> Json.obj(
            "year"  -> 1948,
            "month" -> 4
          ),
          "officer_role" -> "director",
          "address" -> Json.obj(
            "locality"       -> "London",
            "country"        -> "England",
            "address_line_1" -> "9 Charcot Road",
            "premises"       -> "Flat 6, Osler Court",
            "postal_code"    -> "NW9 5XW"
          ),
          "name" -> "FERGUSON, Jim",
          "links" -> Json.obj(
            "officer" -> Json.obj(
              "appointments" -> "/officers/mlvDFJq0QpFX1hTw93U7MJNh_ko/appointments"
            )
          ),
          "nationality" -> "Italian"
        ),
        Json.obj(
          "appointed_on"         -> "2015-04-10",
          "occupation"           -> "Director",
          "country_of_residence" -> "United Kingdom",
          "date_of_birth" -> Json.obj(
            "year"  -> 1948,
            "month" -> 4
          ),
          "officer_role" -> "director",
          "address" -> Json.obj(
            "locality"       -> "London",
            "country"        -> "England",
            "address_line_1" -> "9 Charcot Road",
            "premises"       -> "Flat 6, Osler Court",
            "postal_code"    -> "NW9 5XW"
          ),
          "name" -> "FERGUSON, Jim",
          "links" -> Json.obj(
            "officer" -> Json.obj(
              "appointments" -> "/officers/mlvDFJq0QpFX1hTw93U7MJNh_ko/appointments"
            )
          ),
          "nationality" -> "Italian",
          "resigned_on" -> "1999-01-01"
        )
      ),
      "links" -> Json.obj(
        "self" -> "/company/01234567/appointments"
      ),
      "active_count"   -> 101,
      "total_results"  -> 101,
      "kind"           -> "officer-list",
      "start_index"    -> 0,
      "resigned_count" -> 1
    )

    val officersBodyNoAddress = Json.obj(
      "total_results"  -> 3,
      "items_per_page" -> 35,
      "etag"           -> "0905d15615b770cd4fcc27fdc1c959474ae4c03e",
      "items" -> Json.arr(
        Json.obj(
          "appointed_on"         -> "2015-04-10",
          "occupation"           -> "Director",
          "country_of_residence" -> "United Kingdom",
          "date_of_birth" -> Json.obj(
            "year"  -> 1948,
            "month" -> 4
          ),
          "officer_role" -> "director",
          "name"         -> "FERGUSON, Jim",
          "links" -> Json.obj(
            "officer" -> Json.obj(
              "appointments" -> "/officers/mlvDFJq0QpFX1hTw93U7MJNh_ko/appointments"
            )
          ),
          "nationality" -> "Italian"
        )
      ),
      "links" -> Json.obj(
        "self" -> "/company/01234567/appointments"
      ),
      "active_count"   -> 101,
      "total_results"  -> 101,
      "kind"           -> "officer-list",
      "start_index"    -> 0,
      "resigned_count" -> 1
    )
  }

  lazy val capgeminiOfficers = Json.obj(
    "kind" -> "officer-list",
    "items" -> Json.arr(
      Json.obj(
        "appointed_on" -> "1998-08-13",
        "name"         -> "MANGAN, Julie",
        "address" -> Json.obj(
          "postal_code"    -> "GU21 6DB",
          "address_line_2" -> "Woking",
          "locality"       -> "Surrey",
          "address_line_1" -> "1 Forge End"
        ),
        "nationality"  -> "British",
        "officer_role" -> "secretary",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/6QJfWMjVPzyn1mjrTGrusjYvz84/appointments"
          )
        )
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/myvrsl8CpSsEdtbfNQqBr7KUXus/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 10,
          "year"  -> 1971
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "England",
        "appointed_on"         -> "2020-03-31",
        "occupation"           -> "Ceo, Cloud Infrastructure Services",
        "nationality"          -> "British",
        "address" -> Json.obj(
          "locality"       -> "Surrey",
          "address_line_1" -> "1 Forge End",
          "postal_code"    -> "GU21 6DB",
          "address_line_2" -> "Woking"
        ),
        "name" -> "BHAGAT, Nivedita Krishnamurthy"
      ),
      Json.obj(
        "name"       -> "EZZAT, Aiman",
        "occupation" -> "Chief Financial Officer Of Capgemini Group",
        "address" -> Json.obj(
          "postal_code"    -> "GU21 6DB",
          "locality"       -> "Surrey",
          "address_line_1" -> "1 Forge End",
          "address_line_2" -> "Woking"
        ),
        "nationality"  -> "French",
        "appointed_on" -> "2013-01-02",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/_bZE2Fmu-4ER8JOYqFGT0PL26hs/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 5,
          "year"  -> 1961
        ),
        "country_of_residence" -> "France",
        "officer_role"         -> "director"
      ),
      Json.obj(
        "officer_role"         -> "director",
        "country_of_residence" -> "France",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/2-cMatjiPJ200vERv4xmYsovKhk/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1970,
          "month" -> 4
        ),
        "appointed_on" -> "2019-02-01",
        "nationality"  -> "French",
        "address" -> Json.obj(
          "postal_code"    -> "GU21 6DB",
          "address_line_1" -> "1 Forge End",
          "locality"       -> "Surrey",
          "address_line_2" -> "Woking"
        ),
        "occupation" -> "Group Chief Financial Officer",
        "name"       -> "FERRAND, Carole Gabriella"
      ),
      Json.obj(
        "occupation" -> "Finance Director",
        "address" -> Json.obj(
          "address_line_2" -> "Woking",
          "postal_code"    -> "GU21 6DB",
          "locality"       -> "Surrey",
          "address_line_1" -> "1 Forge End"
        ),
        "nationality" -> "British",
        "name"        -> "HART, Clive David",
        "date_of_birth" -> Json.obj(
          "year"  -> 1973,
          "month" -> 2
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/kcnsAB8RZ4MaPozMVuQN7x04M34/appointments"
          )
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "United Kingdom",
        "appointed_on"         -> "2019-02-01"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/q9X4m4JIo58ivpPkWT0Xlbvg_To/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1966,
          "month" -> 9
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "England",
        "appointed_on"         -> "2019-02-01",
        "occupation"           -> "Managing Director",
        "nationality"          -> "British",
        "address" -> Json.obj(
          "address_line_1" -> "1 Forge End",
          "postal_code"    -> "GU21 6DB",
          "address_line_2" -> "Woking",
          "locality"       -> "Surrey"
        ),
        "name" -> "MARGETTS, Paul Arthur"
      ),
      Json.obj(
        "nationality" -> "British",
        "address" -> Json.obj(
          "postal_code"    -> "GU21 6DB",
          "address_line_2" -> "Woking",
          "address_line_1" -> "1 Forge End",
          "locality"       -> "Surrey"
        ),
        "occupation"           -> "Vice President",
        "name"                 -> "STARK, Rosemary Joy",
        "officer_role"         -> "director",
        "country_of_residence" -> "United Kingdom",
        "date_of_birth" -> Json.obj(
          "month" -> 2,
          "year"  -> 1963
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/mUbGeo2deo3xIhdq3AX-cBYXSZw/appointments"
          )
        ),
        "appointed_on" -> "2016-12-05"
      ),
      Json.obj(
        "address" -> Json.obj(
          "address_line_1" -> "9 Setley Way",
          "locality"       -> "Bracknell",
          "postal_code"    -> "RG12 6QF",
          "region"         -> "Berkshire"
        ),
        "officer_role" -> "secretary",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/9asxn6VYT40-HQsnqYf7bxzpCZ0/appointments"
          )
        ),
        "resigned_on" -> "1993-04-30",
        "name"        -> "MEADES, Derek Leslie"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/tHpXuMRCwxV1B14yWXt-O5mPZ2w/appointments"
          )
        ),
        "address" -> Json.obj(
          "postal_code"    -> "BH13 7PB",
          "region"         -> "Dorset",
          "address_line_1" -> "23 Chaddesley Glen",
          "locality"       -> "Poole"
        ),
        "officer_role" -> "secretary",
        "name"         -> "PICKLES, Christine Mary",
        "appointed_on" -> "1998-01-06",
        "resigned_on"  -> "1998-08-13"
      ),
      Json.obj(
        "name"         -> "WILLIAMS, John Frederick",
        "appointed_on" -> "1993-04-30",
        "resigned_on"  -> "1998-01-06",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/Tb02EPr9Mmfnckl2UlykQEggKhI/appointments"
          )
        ),
        "address" -> Json.obj(
          "address_line_1" -> "22 Inglewood Drive",
          "address_line_2" -> "Nyetimber",
          "postal_code"    -> "PO21 4JX",
          "locality"       -> "Bognor Regis",
          "region"         -> "West Sussex"
        ),
        "officer_role" -> "secretary"
      ),
      Json.obj(
        "resigned_on"  -> "2002-09-09",
        "appointed_on" -> "2000-02-11",
        "date_of_birth" -> Json.obj(
          "year"  -> 1950,
          "month" -> 2
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/ItHVb6p-ybQvOfq3zjZe3d89Y6o/appointments"
          )
        ),
        "officer_role" -> "director",
        "name"         -> "ABELL, Maurice Anthony",
        "occupation"   -> "Ceo",
        "nationality"  -> "British",
        "address" -> Json.obj(
          "address_line_1" -> "21 Saint Regis Heights",
          "address_line_2" -> "Firecrest Drive",
          "locality"       -> "London",
          "postal_code"    -> "NW3 7NE"
        )
      ),
      Json.obj(
        "resigned_on" -> "1993-04-30",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/QLTeRjyo6GTf6pN9-rjEepFIfVg/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1937,
          "month" -> 12
        ),
        "country_of_residence" -> "United Kingdom",
        "officer_role"         -> "director",
        "name"                 -> "BIRCH, Peter Gibbs",
        "occupation"           -> "Director",
        "address" -> Json.obj(
          "locality"       -> "Cobham",
          "region"         -> "Surrey",
          "address_line_1" -> "Bibury 24 Broad High Way",
          "postal_code"    -> "KT11 2RP"
        ),
        "nationality" -> "British"
      ),
      Json.obj(
        "occupation" -> "Cfo",
        "address" -> Json.obj(
          "locality"       -> "Paris",
          "address_line_1" -> "20 Rue Beaujon",
          "country"        -> "France",
          "postal_code"    -> "FOREIGN"
        ),
        "nationality" -> "French",
        "name"        -> "BITAN, William Hai",
        "date_of_birth" -> Json.obj(
          "year"  -> 1949,
          "month" -> 1
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/xhpToE741hTWwQ6-Ms3xzbOisH8/appointments"
          )
        ),
        "officer_role" -> "director",
        "resigned_on"  -> "2004-09-14",
        "appointed_on" -> "2003-05-01"
      ),
      Json.obj(
        "date_of_birth" -> Json.obj(
          "month" -> 10,
          "year"  -> 1920
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/kYKgCFXjmXVJiln9QMmwfwuY41M/appointments"
          )
        ),
        "officer_role" -> "director",
        "resigned_on"  -> "1993-04-30",
        "occupation"   -> "Director",
        "address" -> Json.obj(
          "address_line_2" -> "Tite Hill",
          "locality"       -> "Englefield Green",
          "region"         -> "Surrey",
          "address_line_1" -> "Sunnymead",
          "postal_code"    -> "TW20 0NH"
        ),
        "nationality" -> "British",
        "name"        -> "BOLTON, John Eveleigh, Dr"
      ),
      Json.obj(
        "occupation" -> "Ceo, Capgemini Consulting",
        "address" -> Json.obj(
          "address_line_2" -> "Woking",
          "address_line_1" -> "1 Forge End",
          "locality"       -> "Surrey",
          "postal_code"    -> "GU21 6DB"
        ),
        "nationality" -> "British",
        "name"        -> "COOK, William",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/PDZzyOnqGwKnXE_DPv01w3mjkAA/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1955,
          "month" -> 11
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "United Kingdom",
        "appointed_on"         -> "2012-02-01",
        "resigned_on"          -> "2013-12-31"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/Vq53cDrrnq1yhC9KoyWryG_qni4/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1959,
          "month" -> 8
        ),
        "officer_role" -> "director",
        "resigned_on"  -> "2009-01-01",
        "appointed_on" -> "2004-05-28",
        "occupation"   -> "Director",
        "address" -> Json.obj(
          "address_line_1" -> "Summerhayes",
          "address_line_2" -> "Lunghurst Road Woldingham",
          "postal_code"    -> "CR3 7EJ",
          "locality"       -> "Caterham"
        ),
        "nationality" -> "French",
        "name"        -> "CROS, Pierre Yves"
      ),
      Json.obj(
        "officer_role"         -> "director",
        "country_of_residence" -> "France",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/Z85Jn94iZfA8Fca98R0I6tt_QaE/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 5,
          "year"  -> 1956
        ),
        "appointed_on" -> "2005-06-24",
        "resigned_on"  -> "2005-10-10",
        "address" -> Json.obj(
          "locality"       -> "Paris",
          "country"        -> "France",
          "address_line_2" -> "Neuilly S/Seine 92200",
          "premises"       -> "30",
          "address_line_1" -> "Boulevard Victor Hugo"
        ),
        "nationality" -> "French",
        "occupation"  -> "Coo",
        "name"        -> "DANON, Pierre"
      ),
      Json.obj(
        "appointed_on"         -> "2009-01-01",
        "resigned_on"          -> "2018-01-22",
        "officer_role"         -> "director",
        "country_of_residence" -> "England",
        "date_of_birth" -> Json.obj(
          "month" -> 1,
          "year"  -> 1959
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/Gr_Sz8dszmvSh3Mo-ZMSC4agzao/appointments"
          )
        ),
        "name"        -> "DEANS, Anthony John",
        "nationality" -> "British",
        "address" -> Json.obj(
          "address_line_1" -> "1 Forge End",
          "postal_code"    -> "GU21 6DB",
          "locality"       -> "Surrey",
          "address_line_2" -> "Woking"
        ),
        "occupation" -> "Chief Financial Officer"
      ),
      Json.obj(
        "occupation"  -> "Director",
        "nationality" -> "French",
        "address" -> Json.obj(
          "region"         -> "Cr3 7ej",
          "locality"       -> "912 Athis Mons",
          "address_line_1" -> "12 Rue D'Ablon",
          "country"        -> "France"
        ),
        "name" -> "DONZEAUD, Alain",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/bbjqu1JoT1Q0Vi0MzmvSKLIR730/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 11,
          "year"  -> 1948
        ),
        "country_of_residence" -> "France",
        "officer_role"         -> "director",
        "appointed_on"         -> "2004-05-28",
        "resigned_on"          -> "2012-06-30"
      ),
      Json.obj(
        "resigned_on"          -> "2012-11-08",
        "appointed_on"         -> "2004-09-30",
        "country_of_residence" -> "France",
        "officer_role"         -> "director",
        "date_of_birth" -> Json.obj(
          "month" -> 7,
          "year"  -> 1963
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/Daz5gMEUdLMj9UzWt0kSXlgefoI/appointments"
          )
        ),
        "name"        -> "DUFOURCQ, Nicolas",
        "nationality" -> "French",
        "address" -> Json.obj(
          "address_line_1" -> "42 Rue Hanri Barbusse",
          "postal_code"    -> "FOREIGN",
          "locality"       -> "Paris",
          "country"        -> "Frances",
          "region"         -> "75005"
        ),
        "occupation" -> "Cfo Capgemini Group"
      ),
      Json.obj(
        "officer_role" -> "director",
        "date_of_birth" -> Json.obj(
          "month" -> 4,
          "year"  -> 1943
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/mwoVQRiLleNPIw8kEP1FrTP4S2k/appointments"
          )
        ),
        "resigned_on" -> "1996-04-30",
        "nationality" -> "British",
        "address" -> Json.obj(
          "locality"       -> "Shepperton",
          "postal_code"    -> "TW17 8LQ",
          "region"         -> "Middlesex",
          "address_line_1" -> "Dunally House Walton Lane"
        ),
        "occupation" -> "Director",
        "name"       -> "FISHER, Anthony Frederick"
      ),
      Json.obj(
        "occupation"  -> "Finance Director",
        "nationality" -> "British",
        "address" -> Json.obj(
          "locality"       -> "Surrey",
          "address_line_1" -> "1 Forge End",
          "address_line_2" -> "Woking",
          "postal_code"    -> "GU21 6DB"
        ),
        "name" -> "GILSHENAN, James Robert Peter",
        "date_of_birth" -> Json.obj(
          "month" -> 11,
          "year"  -> 1965
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/S8ETIZCGtuaX7YMsAMz1b3M-aYw/appointments"
          )
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "United Kingdom",
        "resigned_on"          -> "2019-02-01",
        "appointed_on"         -> "2018-01-18"
      ),
      Json.obj(
        "date_of_birth" -> Json.obj(
          "month" -> 6,
          "year"  -> 1957
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/-3riFDu-LG6fuyNxfFxa8pjYwnY/appointments"
          )
        ),
        "officer_role" -> "director",
        "resigned_on"  -> "2003-05-01",
        "appointed_on" -> "2002-09-09",
        "occupation"   -> "Vice President",
        "nationality"  -> "French",
        "address" -> Json.obj(
          "postal_code"    -> "FOREIGN",
          "region"         -> "75007",
          "country"        -> "France",
          "address_line_1" -> "87 Boulevard Du Montparnasse",
          "locality"       -> "Paris"
        ),
        "name" -> "GIRAUD, Hubert Paul Henri"
      ),
      Json.obj(
        "address" -> Json.obj(
          "locality"       -> "Paris",
          "region"         -> "75017",
          "address_line_1" -> "11 Rue De Tilsitt",
          "country"        -> "France",
          "postal_code"    -> "FOREIGN"
        ),
        "nationality"  -> "French",
        "occupation"   -> "Director",
        "name"         -> "HAEFFNER, Alexandre",
        "officer_role" -> "director",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/eY-Qk_7BW5q1ZOLzxjoL6V7-U6M/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1944,
          "month" -> 9
        ),
        "appointed_on" -> "2002-09-09",
        "resigned_on"  -> "2005-06-24"
      ),
      Json.obj(
        "name"        -> "HARSANT, Raymond Peter",
        "nationality" -> "British",
        "address" -> Json.obj(
          "region"         -> "Kent",
          "postal_code"    -> "TN13 2LN",
          "locality"       -> "Sevenoaks",
          "address_line_1" -> "Beech Hill 69 Kippington Road"
        ),
        "occupation"           -> "Chartered Accountant",
        "resigned_on"          -> "1993-04-30",
        "country_of_residence" -> "United Kingdom",
        "officer_role"         -> "director",
        "date_of_birth" -> Json.obj(
          "month" -> 7,
          "year"  -> 1937
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/zn8trEa1UhTg7iFsP7PLeJD3-hI/appointments"
          )
        )
      ),
      Json.obj(
        "name"        -> "HERMELIN, Paul",
        "occupation"  -> "Member Of The Directoire",
        "nationality" -> "French",
        "address" -> Json.obj(
          "address_line_2" -> "Paris",
          "locality"       -> "75116",
          "region"         -> "France",
          "address_line_1" -> "60 Avenue Raymond Poinacre",
          "postal_code"    -> "FOREIGN"
        ),
        "resigned_on"  -> "2002-09-09",
        "appointed_on" -> "1999-06-15",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/fCm-sLSf5_K-yGiP7QlEWDYzO1w/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 4,
          "year"  -> 1952
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "France"
      ),
      Json.obj(
        "name" -> "HESSLER, Pierre",
        "address" -> Json.obj(
          "locality"       -> "France 75007",
          "address_line_1" -> "21 Rue Monsieur",
          "address_line_2" -> "Paris"
        ),
        "nationality"  -> "Swiss",
        "occupation"   -> "Deputy General Manager",
        "appointed_on" -> "1994-03-15",
        "resigned_on"  -> "2001-11-08",
        "officer_role" -> "director",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/FxNVDi_u9S-xMPIsmx_tkhjjH8Y/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 10,
          "year"  -> 1941
        )
      ),
      Json.obj(
        "name"        -> "HODGSON, Christine Mary",
        "occupation"  -> "Ceo Technology Services North West",
        "nationality" -> "British",
        "address" -> Json.obj(
          "postal_code"    -> "GU21 6DB",
          "address_line_2" -> "Woking",
          "address_line_1" -> "1 Forge End",
          "locality"       -> "Surrey"
        ),
        "resigned_on"  -> "2020-03-31",
        "appointed_on" -> "1998-07-29",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/tHpXuMRCwxV1B14yWXt-O5mPZ2w/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1964,
          "month" -> 11
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "England"
      ),
      Json.obj(
        "address" -> Json.obj(
          "address_line_2" -> "Paris 75016",
          "address_line_1" -> "2 Avenue D'Iena",
          "locality"       -> "France"
        ),
        "nationality"  -> "French",
        "occupation"   -> "Director",
        "name"         -> "JALABERT, Michael Francis",
        "officer_role" -> "director",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/LL2S103tXecpZCBZ-z0DNha_rsw/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1933,
          "month" -> 1
        ),
        "resigned_on" -> "1999-06-15"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/ni8-5fuwEYNUVjqlX2_LauF4kjM/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 10,
          "year"  -> 1934
        ),
        "officer_role" -> "director",
        "resigned_on"  -> "2001-11-08",
        "occupation"   -> "Director",
        "address" -> Json.obj(
          "country"        -> "France",
          "postal_code"    -> "FOREIGN",
          "locality"       -> "Paris 75017",
          "address_line_1" -> "11 Rue De Tilsitt"
        ),
        "nationality" -> "French",
        "name"        -> "KAMPF, Serge Paul Jean"
      ),
      Json.obj(
        "appointed_on" -> "2001-11-08",
        "resigned_on"  -> "2002-09-09",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/k05LuJSksoB7DZz8fO2j3aU8GuQ/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1965,
          "month" -> 6
        ),
        "officer_role" -> "director",
        "name"         -> "LEMOINE, Frederick Nicholas",
        "occupation"   -> "Cfo",
        "nationality"  -> "French",
        "address" -> Json.obj(
          "locality"       -> "Paris",
          "address_line_1" -> "11 Avenue De La Bourdonnais",
          "country"        -> "France",
          "postal_code"    -> "FOREIGN",
          "region"         -> "75007"
        )
      ),
      Json.obj(
        "occupation"  -> "Group General Secretary",
        "nationality" -> "French",
        "address" -> Json.obj(
          "address_line_2" -> "Woking",
          "postal_code"    -> "GU21 6DB",
          "address_line_1" -> "1 Forge End",
          "locality"       -> "Surrey"
        ),
        "name" -> "MASSIGNON, Jean-Baptiste Jacques Emmanuel Valery",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/0d05aG6_uDfiWv-N2j16Ckm0eHY/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 7,
          "year"  -> 1964
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "France",
        "resigned_on"          -> "2019-02-01",
        "appointed_on"         -> "2012-09-03"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/TQwkYIhRfxypozm-6wIQghVCOic/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1959,
          "month" -> 7
        ),
        "officer_role" -> "director",
        "resigned_on"  -> "2004-05-28",
        "appointed_on" -> "2003-05-01",
        "occupation"   -> "Director",
        "nationality"  -> "American",
        "address" -> Json.obj(
          "region"         -> "Texas 75034",
          "country"        -> "Usa",
          "address_line_1" -> "4808 Pinehurst Drive",
          "locality"       -> "Frisco"
        ),
        "name" -> "MCCAIN, John William"
      ),
      Json.obj(
        "name"       -> "MEADES, Derek Leslie",
        "occupation" -> "Accountant",
        "address" -> Json.obj(
          "region"         -> "Berkshire",
          "postal_code"    -> "SL5 7LY",
          "address_line_2" -> "The Avenue",
          "locality"       -> "Ascot",
          "address_line_1" -> "Alfriston House"
        ),
        "nationality"  -> "British",
        "resigned_on"  -> "1998-07-29",
        "appointed_on" -> "1993-04-30",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/9asxn6VYT40-HQsnqYf7bxzpCZ0/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1953,
          "month" -> 1
        ),
        "officer_role" -> "director"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/6OBi8AIxlh12qgjVoPKCEaZpP4I/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1959,
          "month" -> 11
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "Ireland",
        "appointed_on"         -> "2009-01-01",
        "resigned_on"          -> "2017-04-06",
        "occupation"           -> "Ceo Of Continental Europe And Asia",
        "nationality"          -> "British",
        "address" -> Json.obj(
          "locality"       -> "Surrey",
          "address_line_1" -> "1 Forge End",
          "postal_code"    -> "GU21 6DB",
          "address_line_2" -> "Woking"
        ),
        "name" -> "NANNETTI, Paul Anthony"
      ),
      Json.obj(
        "appointed_on" -> "2005-12-19",
        "resigned_on"  -> "2009-01-01",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/8nqlZByNmlrRi4Ztzg57fKS1lpg/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1958,
          "month" -> 11
        ),
        "officer_role" -> "director",
        "name"         -> "NICOLET, Patrick Michel",
        "occupation"   -> "Head Of Global Sales And Allia",
        "nationality"  -> "Swiss",
        "address" -> Json.obj(
          "country"        -> "Switzerland",
          "address_line_2" -> "Ch,",
          "address_line_1" -> "Chemin De Haute Brise 10,",
          "locality"       -> "1012 Lausanne",
          "postal_code"    -> "FOREIGN"
        )
      ),
      Json.obj(
        "resigned_on"          -> "2010-10-29",
        "appointed_on"         -> "2009-01-01",
        "country_of_residence" -> "France",
        "officer_role"         -> "director",
        "date_of_birth" -> Json.obj(
          "year"  -> 1962,
          "month" -> 3
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/xLZVnGfq9bt4q2pnTkN1lSYWIg8/appointments"
          )
        ),
        "name"        -> "PICARD, Olivier Gaston",
        "nationality" -> "French",
        "address" -> Json.obj(
          "premises"       -> "272",
          "country"        -> "France",
          "postal_code"    -> "75007",
          "locality"       -> "Paris",
          "address_line_1" -> "Bd. St. Germain"
        ),
        "occupation" -> "Chief Sales Officer"
      ),
      Json.obj(
        "officer_role"         -> "director",
        "country_of_residence" -> "England",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/6ayHMKWCXvLkqvB9iQpluQz4La8/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 4,
          "year"  -> 1955
        ),
        "resigned_on"  -> "2009-01-01",
        "appointed_on" -> "2004-12-31",
        "address" -> Json.obj(
          "postal_code"    -> "SO21 1AX",
          "region"         -> "Hampshire",
          "address_line_1" -> "The Down House",
          "address_line_2" -> "Main Road",
          "locality"       -> "Itchen Abbas"
        ),
        "nationality" -> "British",
        "occupation"  -> "Chief Executive Officer",
        "name"        -> "PORTER, Mark Stephen"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/AJeL0WWmxDrXrEiCJRhBkkGW1rE/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 1,
          "year"  -> 1942
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "United Kingdom",
        "resigned_on"          -> "1999-02-01",
        "occupation"           -> "Director",
        "address" -> Json.obj(
          "address_line_2" -> "49a Britannia Road",
          "address_line_1" -> "2 Britannia Studios",
          "postal_code"    -> "SW6 2HJ",
          "locality"       -> "London"
        ),
        "nationality" -> "British",
        "name"        -> "ROBINSON, Anthony Charles Evitt"
      ),
      Json.obj(
        "date_of_birth" -> Json.obj(
          "year"  -> 1957,
          "month" -> 3
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/3ENZuJR9HKGoLMdVOfRK5JK5F8A/appointments"
          )
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "France",
        "appointed_on"         -> "2016-12-05",
        "resigned_on"          -> "2019-02-01",
        "occupation"           -> "Head Of Group Commercial & Contract Management",
        "address" -> Json.obj(
          "address_line_2" -> "Woking",
          "address_line_1" -> "1 Forge End",
          "postal_code"    -> "GU21 6DB",
          "locality"       -> "Surrey"
        ),
        "nationality" -> "French",
        "name"        -> "ROUX-CHENU, Isabelle"
      ),
      Json.obj(
        "date_of_birth" -> Json.obj(
          "year"  -> 1959,
          "month" -> 1
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/GRU8zDQ_Q7Lba7GpM96SJTsB2EM/appointments"
          )
        ),
        "country_of_residence" -> "United Kingdom",
        "officer_role"         -> "director",
        "resigned_on"          -> "2012-01-13",
        "appointed_on"         -> "2002-08-01",
        "occupation"           -> "Director",
        "nationality"          -> "American",
        "address" -> Json.obj(
          "country"        -> "United Kingdom",
          "region"         -> "Surrey",
          "postal_code"    -> "GU25 4PL",
          "premises"       -> "Woodlands",
          "locality"       -> "Virginia Water",
          "address_line_1" -> "Woodlands Road West"
        ),
        "name" -> "SPENCE, Paul David"
      ),
      Json.obj(
        "name" -> "UGANDER, Christer",
        "address" -> Json.obj(
          "locality"       -> "Chavenay F78450",
          "address_line_1" -> "3 Rue Des Bouleaux",
          "country"        -> "France",
          "postal_code"    -> "FOREIGN"
        ),
        "nationality"  -> "Swedish",
        "occupation"   -> "Director",
        "resigned_on"  -> "1994-03-31",
        "officer_role" -> "director",
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/rf8ivVbdK21cC3uh2FoAKUCgYPA/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "month" -> 9,
          "year"  -> 1934
        )
      ),
      Json.obj(
        "resigned_on"          -> "2002-09-09",
        "officer_role"         -> "director",
        "country_of_residence" -> "England",
        "date_of_birth" -> Json.obj(
          "year"  -> 1942,
          "month" -> 8
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/LYp3Yo5CgoVK_M09_9lRQufvmrg/appointments"
          )
        ),
        "name" -> "UNWIN, Eric Geoffrey",
        "address" -> Json.obj(
          "postal_code"    -> "NW1 4AE",
          "address_line_1" -> "17 Park Village West",
          "locality"       -> "London"
        ),
        "nationality" -> "British",
        "occupation"  -> "Executive Chairman"
      ),
      Json.obj(
        "name"        -> "WELCH, Alwyn Frank",
        "nationality" -> "British",
        "address" -> Json.obj(
          "address_line_1" -> "Coniston Farnham Lane",
          "locality"       -> "Haslemere",
          "postal_code"    -> "GU27 1EZ",
          "region"         -> "Surrey"
        ),
        "occupation"           -> "Company Director",
        "resigned_on"          -> "2000-02-11",
        "appointed_on"         -> "1998-02-02",
        "officer_role"         -> "director",
        "country_of_residence" -> "England",
        "date_of_birth" -> Json.obj(
          "year"  -> 1957,
          "month" -> 7
        ),
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/WVCkctQ3QqeR8LkHDwN4gAXl1aE/appointments"
          )
        )
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/WIXICq5y4VecNEFsq5FeE35HmX8/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1944,
          "month" -> 8
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "England",
        "resigned_on"          -> "2004-12-31",
        "appointed_on"         -> "2004-05-28",
        "occupation"           -> "Managing Director",
        "nationality"          -> "British",
        "address" -> Json.obj(
          "address_line_1" -> "13 Belvedere Avenue",
          "locality"       -> "London",
          "postal_code"    -> "SW19 7PP"
        ),
        "name" -> "WILLIAMS, Clive Robert"
      ),
      Json.obj(
        "links" -> Json.obj(
          "officer" -> Json.obj(
            "appointments" -> "/officers/d87MMx4k_tZb2AbgmJvqJxkLEO0/appointments"
          )
        ),
        "date_of_birth" -> Json.obj(
          "year"  -> 1966,
          "month" -> 3
        ),
        "officer_role"         -> "director",
        "country_of_residence" -> "United Kingdom",
        "resigned_on"          -> "2019-02-01",
        "appointed_on"         -> "2013-12-31",
        "occupation"           -> "Director",
        "address" -> Json.obj(
          "address_line_1" -> "1 Forge End",
          "postal_code"    -> "GU21 6DB",
          "locality"       -> "Surrey",
          "address_line_2" -> "Woking"
        ),
        "nationality" -> "British",
        "name"        -> "WILLIAMS, David Evan Huw"
      )
    ),
    "total_results"  -> 46,
    "items_per_page" -> 47,
    "links" -> Json.obj(
      "self" -> "/company/00943935/officers"
    ),
    "start_index"    -> 0,
    "etag"           -> "6ece49b03182a1f2a0f67b9be98ae10baf4fba84",
    "resigned_count" -> 39,
    "active_count"   -> 7
  )
}
