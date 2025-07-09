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

package uk.gov.hmrc.gform.companieshouse

import java.time.LocalDate
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers.stubControllerComponents
import uk.gov.hmrc.gform.companieshouse.models.{ Address, Item, ItemLinks, Links, Officer, OfficersResponse }
import uk.gov.hmrc.http.{ BadGatewayException, ForbiddenException, GatewayTimeoutException, NotFoundException, UnauthorizedException }

import scala.concurrent.Future

class CompaniesHouseControllerSpec extends UnitSpec {

  val cc = stubControllerComponents()

  private trait Setup {
    val mockCompaniesHouseService: CompaniesHouseService = mock[CompaniesHouseService]
    val companiesHouseController: CompaniesHouseController = new CompaniesHouseController(mockCompaniesHouseService, cc)

    val companyNumber: String = "number01"
    val jsonBody = Json.obj("test" -> "testvalue")
  }

  "findCompany" should {
    "return Ok and response body if find successful" in new Setup {
      when(mockCompaniesHouseService.findCompany(any[String])(any[Request[_]])) `thenReturn` Future.successful(jsonBody)
      val result = await(companiesHouseController.findCompany(companyNumber)(FakeRequest()))
      status(result) shouldBe Status.OK
      contentAsJson(result) shouldBe jsonBody
    }

    "return NOT_FOUND if find successful but returns NOT_FOUND" in new Setup {
      when(mockCompaniesHouseService.findCompany(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new NotFoundException("some message")
      )
      val result = await(companiesHouseController.findCompany(companyNumber)(FakeRequest()))
      status(result) shouldBe Status.NOT_FOUND
    }

    "return UNAUTHORIZED if find successful but returns UNAUTHORIZED" in new Setup {
      when(mockCompaniesHouseService.findCompany(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new UnauthorizedException("some message")
      )

      val result = await(companiesHouseController.findCompany(companyNumber)(FakeRequest()))
      status(result) shouldBe Status.UNAUTHORIZED
    }

    "return FORBIDDEN if find successful but returns FORBIDDEN" in new Setup {
      when(mockCompaniesHouseService.findCompany(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new ForbiddenException("some message")
      )

      val result = await(companiesHouseController.findCompany(companyNumber)(FakeRequest()))
      status(result) shouldBe Status.FORBIDDEN
    }

    "return GATEWAY_TIMEOUT when a TimeoutException thrown" in new Setup {
      when(mockCompaniesHouseService.findCompany(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new GatewayTimeoutException("Game Over Man")
      )
      val result = await(companiesHouseController.findCompany(companyNumber)(FakeRequest()))
      status(result) shouldBe Status.GATEWAY_TIMEOUT
    }

    "return BadGateway when a ConnectException thrown" in new Setup {
      when(mockCompaniesHouseService.findCompany(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new BadGatewayException("Mutually agreed uncoupling")
      )
      val result = await(companiesHouseController.findCompany(companyNumber)(FakeRequest()))
      status(result) shouldBe Status.BAD_GATEWAY
    }

    "return ServiceUnavailable when any other exception thrown" in new Setup {
      when(mockCompaniesHouseService.findCompany(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new IllegalArgumentException("I told you that bird was sick but would you listen")
      )
      val result = await(companiesHouseController.findCompany(companyNumber)(FakeRequest()))
      status(result) shouldBe Status.SERVICE_UNAVAILABLE
    }
  }

  "findCompanyOfficers" should {
    "return Ok and response body if find successful" in new Setup {
      val response = OfficersResponse(0, "", Nil, 0, "", Links(""), 0, 0, None)

      when(mockCompaniesHouseService.findCompanyOfficers(any[String])(any[Request[_]])) `thenReturn` Future.successful(
        response
      )
      val result = await(companiesHouseController.findCompanyOfficers(companyNumber, None)(FakeRequest()))
      status(result) shouldBe Status.OK
      contentAsJson(result) shouldBe Json.obj(
        "active_count"   -> 0,
        "etag"           -> "",
        "items"          -> Json.arr(),
        "items_per_page" -> 0,
        "kind"           -> "",
        "links" -> Json.obj(
          "self" -> ""
        ),
        "resigned_count" -> 0,
        "start_index"    -> 0
      )
    }

    "return NOT_FOUND if find successful but returns NOT_FOUND" in new Setup {
      when(mockCompaniesHouseService.findCompanyOfficers(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new NotFoundException("some message")
      )
      val result = await(companiesHouseController.findCompanyOfficers(companyNumber, None)(FakeRequest()))
      status(result) shouldBe Status.NOT_FOUND
    }

    "return UNAUTHORIZED if find successful but returns UNAUTHORIZED" in new Setup {
      when(mockCompaniesHouseService.findCompanyOfficers(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new UnauthorizedException("some message")
      )
      val result = await(companiesHouseController.findCompanyOfficers(companyNumber, None)(FakeRequest()))
      status(result) shouldBe Status.UNAUTHORIZED
    }

    "return FORBIDDEN if find successful but returns FORBIDDEN" in new Setup {
      when(mockCompaniesHouseService.findCompanyOfficers(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new ForbiddenException("some message")
      )
      val result = await(companiesHouseController.findCompanyOfficers(companyNumber, None)(FakeRequest()))
      status(result) shouldBe Status.FORBIDDEN
    }

    "return GATEWAY_TIMEOUT when a TimeoutException thrown" in new Setup {
      when(mockCompaniesHouseService.findCompanyOfficers(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new GatewayTimeoutException("Game Over Man")
      )
      val result = await(companiesHouseController.findCompanyOfficers(companyNumber, None)(FakeRequest()))
      status(result) shouldBe Status.GATEWAY_TIMEOUT
    }

    "return BadGateway when a ConnectException thrown" in new Setup {
      when(mockCompaniesHouseService.findCompanyOfficers(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new BadGatewayException("Mutually agreed uncoupling")
      )
      val result = await(companiesHouseController.findCompanyOfficers(companyNumber, None)(FakeRequest()))
      status(result) shouldBe Status.BAD_GATEWAY
    }

    "return ServiceUnavailable when any other exception thrown" in new Setup {
      when(mockCompaniesHouseService.findCompanyOfficers(any[String])(any[Request[_]])) `thenReturn` Future.failed(
        new IllegalArgumentException("I told you that bird was sick but would you listen")
      )
      val result = await(companiesHouseController.findCompanyOfficers(companyNumber, None)(FakeRequest()))
      status(result) shouldBe Status.SERVICE_UNAVAILABLE
    }

    "return Ok and response body if find successful when searching by surname" in new Setup {
      val address =
        Address(Some("someAddress1"), None, None, Some("someCounty"), Some("someLocality"), None, None, None, None)
      val appointedOn = LocalDate.parse("2020-06-22")
      val itemLinks = ItemLinks(Officer("someAppointment"))
      val validOfficer = List(
        Item(
          Some(address),
          Some(appointedOn),
          None,
          None,
          None,
          None,
          itemLinks,
          "VALID, name",
          None,
          None,
          "someRole",
          None
        )
      )
      val response = OfficersResponse(0, "", validOfficer, 0, "", Links(""), 0, 0, None)

      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.successful(
        response
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.OK
      contentAsJson(result) shouldBe Json.parse(
        """{"active_count":0,"etag":"","items":[{"address":{"address_line_1":"someAddress1","country":"someCounty","locality":"someLocality"},"appointed_on":"2020-06-22","links":{"officer":{"appointments":"someAppointment"}},"name":"VALID, name","officer_role":"someRole"}],"items_per_page":0,"kind":"","links":{"self":""},"resigned_count":0,"start_index":0}"""
      )
    }

    "return NotFound if the Items list is empty when searching by surname" in new Setup {
      val response = OfficersResponse(0, "", Nil, 0, "", Links(""), 0, 0, None)

      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.successful(
        response
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.NOT_FOUND
    }

    "return NOT_FOUND if find successful but returns NOT_FOUND when searching by surname" in new Setup {
      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.failed(
        new NotFoundException("some message")
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.NOT_FOUND
    }

    "return UNAUTHORIZED if find successful but returns UNAUTHORIZED when searching by surname" in new Setup {
      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.failed(
        new UnauthorizedException("some message")
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.UNAUTHORIZED
    }

    "return FORBIDDEN if find successful but returns FORBIDDEN when searching by surname" in new Setup {
      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.failed(
        new ForbiddenException("some message")
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.FORBIDDEN
    }

    "return GATEWAY_TIMEOUT when a TimeoutException thrown when searching by surname" in new Setup {
      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.failed(
        new GatewayTimeoutException("Game Over Man")
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.GATEWAY_TIMEOUT
    }

    "return BadGateway when a ConnectException thrown when searching by surname" in new Setup {
      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.failed(
        new BadGatewayException("Mutually agreed uncoupling")
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.BAD_GATEWAY
    }

    "return ServiceUnavailable when any other exception thrown when searching by surname" in new Setup {
      when(
        mockCompaniesHouseService.findCompanyOfficersBySurname(any[String], any[String])(any[Request[_]])
      ) `thenReturn` Future.failed(
        new IllegalArgumentException("I told you that bird was sick but would you listen")
      )
      val result =
        await(companiesHouseController.findCompanyOfficers(companyNumber, Some("someSurname"))(FakeRequest()))
      status(result) shouldBe Status.SERVICE_UNAVAILABLE
    }
  }
}
