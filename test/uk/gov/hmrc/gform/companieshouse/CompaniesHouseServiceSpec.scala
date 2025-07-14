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

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import play.api.Configuration
import play.api.mvc.Request
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.companieshouse.models.{ Address, Item, ItemLinks, Links, Officer, OfficersResponse }
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.Future

class CompaniesHouseServiceSpec extends UnitSpec {

  "findCompanyOfficerBySurname" should {
    "Return an officers response with an empty Items list if no officers are returned" in new Setup {
      val response = OfficersResponse(0, "", Nil, 0, "", Links(""), 0, 0, None)

      when(mockCompaniesHouseConnector.getCompanyOfficersPaged(any[String], any[String])(any[Int])(any[Request[_]]))
        .thenReturn(Future.successful(response))
      when(
        mockAuditing.successfulCompanyOfficersResponse(any[OfficersResponse], any[String], any[Option[String]])(
          any[Request[_]]
        )
      ).thenReturn(Future.unit)

      val result = await(service.findCompanyOfficersBySurname("someCompanyNumber", "Testing")(FakeRequest()))
      result.items shouldBe Nil
    }

    "Return an officers response with an empty Items list if officers are returned but not matching the surname" in new Setup {
      val officers = List(
        Item(
          Some(address),
          Some(appointedOn),
          None,
          None,
          None,
          None,
          itemLinks,
          "INVALID, name",
          None,
          None,
          "someRole",
          None
        )
      )
      val response = OfficersResponse(0, "", officers, 0, "", Links(""), 0, 0, None)

      when(mockCompaniesHouseConnector.getCompanyOfficersPaged(any[String], any[String])(any[Int])(any[Request[_]]))
        .thenReturn(Future.successful(response))
      when(
        mockAuditing.successfulCompanyOfficersResponse(any[OfficersResponse], any[String], any[Option[String]])(
          any[Request[_]]
        )
      ).thenReturn(Future.unit)

      val result = await(service.findCompanyOfficersBySurname("someCompanyNumber", "Testing")(FakeRequest()))
      result.items shouldBe Nil
    }

    "Return an officers response with an empty Items list if the officers are returned, surname matching but retired date exists" in new Setup {
      val officers =
        List(
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
            Some(LocalDate.now)
          )
        )
      val response = OfficersResponse(0, "", officers, 0, "", Links(""), 0, 0, None)

      when(mockCompaniesHouseConnector.getCompanyOfficersPaged(any[String], any[String])(any[Int])(any[Request[_]]))
        .thenReturn(Future.successful(response))
      when(
        mockAuditing.successfulCompanyOfficersResponse(any[OfficersResponse], any[String], any[Option[String]])(
          any[Request[_]]
        )
      ).thenReturn(Future.unit)

      val result = await(service.findCompanyOfficersBySurname("someCompanyNumber", "valid")(FakeRequest()))
      result.items shouldBe Nil
    }

    "Return an officers response with a list of Items if the officers are returned, surname matched and retired is empty" in new Setup {
      val officers = List(
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
      val response = OfficersResponse(0, "", officers, 0, "", Links(""), 0, 0, None)

      when(mockCompaniesHouseConnector.getCompanyOfficersPaged(any[String], any[String])(any[Int])(any[Request[_]]))
        .thenReturn(Future.successful(response))
      when(
        mockAuditing.successfulCompanyOfficersResponse(any[OfficersResponse], any[String], any[Option[String]])(
          any[Request[_]]
        )
      ).thenReturn(Future.unit)

      val result = await(service.findCompanyOfficersBySurname("someCompanyNumber", "valid")(FakeRequest()))
      result.items.size shouldBe 1
      result.items.head.name shouldBe "VALID, name"
    }

    "Return an officers response with a list of Items of all the officers that match the surname" in new Setup {
      val first100 = (0 until 100).map { _ =>
        Item(
          Some(address),
          Some(appointedOn),
          None,
          None,
          None,
          None,
          itemLinks,
          "INVALID, name",
          None,
          None,
          "someRole",
          None
        )
      }.toList
      val officer101 = List(
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
      val firstResponse = OfficersResponse(0, "", first100, 0, "", Links(""), 0, 0, Some(101))
      val secondResponse = OfficersResponse(0, "", officer101, 0, "", Links(""), 0, 0, Some(101))

      when(mockCompaniesHouseConnector.getCompanyOfficersPaged(any[String], any[String])(any[Int])(any[Request[_]]))
        .thenReturn(Future.successful(firstResponse))
        .thenReturn(Future.successful(secondResponse))
      when(
        mockAuditing.successfulCompanyOfficersResponse(any[OfficersResponse], any[String], any[Option[String]])(
          any[Request[_]]
        )
      ).thenReturn(Future.unit)

      val result = await(service.findCompanyOfficersBySurname("someCompanyNumber", "valid")(FakeRequest()))
      result.items.size shouldBe 1
      result.items.head.name shouldBe "VALID, name"
    }
  }

  trait Setup {
    val mockCompaniesHouseConnector = mock[CompaniesHouseConnector]
    val mockConfiguration = mock[Configuration]
    val mockAuditing = mock[CompaniesHouseAuditService]

    val address =
      Address(Some("someAddress1"), None, None, Some("someCounty"), Some("someLocality"), None, None, None, None)
    val appointedOn = LocalDate.now()
    val itemLinks = ItemLinks(Officer("someAppointment"))

    val service = new CompaniesHouseService(mockCompaniesHouseConnector, mockConfiguration, mockAuditing) {
      override def withCircuitBreaker[T](f: => Future[T])(implicit hc: HeaderCarrier): Future[T] = f
    }
  }

}
