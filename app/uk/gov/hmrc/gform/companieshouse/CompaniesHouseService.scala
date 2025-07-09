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

import models.{ Item, OfficersResponse }
import play.api.Configuration
import play.api.libs.json.JsValue
import play.api.mvc.Request
import uk.gov.hmrc.circuitbreaker.{ CircuitBreakerConfig, UsingCircuitBreaker }
import uk.gov.hmrc.http.{ BadRequestException, NotFoundException }
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider

import scala.concurrent.{ ExecutionContext, Future }

class CompaniesHouseService(
  connector: CompaniesHouseConnector,
  configuration: Configuration,
  auditing: CompaniesHouseAuditService
)(implicit ec: ExecutionContext)
    extends UsingCircuitBreaker with BackendHeaderCarrierProvider {

  def findCompany(companyNumber: String)(implicit request: Request[?]): Future[JsValue] =
    withCircuitBreaker {
      connector.getCompany(companyNumber)
    }

  def findCompanyOfficers(companyNumber: String)(implicit request: Request[?]): Future[OfficersResponse] =
    withCircuitBreaker {
      connector.getCompanyOfficers(companyNumber).map { response =>
        auditing.successfulCompanyOfficersResponse(response, companyNumber, surnameFilter = None)
//        metrics.recordReturnedOfficers(response.items.length)

        response
      }
    }

  def findCompanyOfficersBySurname(companyNumber: String, surname: String)(implicit
    request: Request[?]
  ): Future[OfficersResponse] =
    withCircuitBreaker {
      connector.getCompanyOfficersPaged(companyNumber, surname)(0).flatMap { initialResult =>
        val indexes = 100 to initialResult.totalResults.getOrElse(0) by 100

        val collectedResponses = indexes.foldLeft(Future.successful(Seq(initialResult)))((current, index) =>
          for {
            currentResult <- current
            newResult     <- connector.getCompanyOfficersPaged(companyNumber, surname)(index)
          } yield currentResult ++ Seq(newResult)
        )

        collectedResponses.map(response => findSpecificActiveOfficer(surname, response)).map { matchingOfficers =>
          val updated = initialResult.copy(items = matchingOfficers)
          auditing.successfulCompanyOfficersResponse(updated, companyNumber, Some(surname))
//          metrics.recordPageHits(updated.totalResults.map(_ / 100).getOrElse(0) + 1)
//          metrics.recordReturnedOfficers(matchingOfficers.length)

          updated
        }
      }
    }

  private def findSpecificActiveOfficer(surname: String, responses: Seq[OfficersResponse]): List[Item] =
    responses.flatMap { response =>
      response.items.filter { value =>
        value.resignedOn.isEmpty &&
        value.name.startsWith(s"${surname.toUpperCase},")
      }
    }.toList

  override def breakOnException(t: Throwable): Boolean = t match {
    case _: NotFoundException | _: BadRequestException => false
    case _                                             => true
  }

  override protected def circuitBreakerConfig: CircuitBreakerConfig = CircuitBreakerConfig(
    "gforms-companies-house",
    configuration.get[Int]("circuit.breaker.numberOfCallsToTrigger"),
    configuration.get[Int]("circuit.breaker.unavailablePeriodDurationInSec") * 1000,
    configuration.get[Int]("circuit.breaker.unstablePeriodDurationInSec") * 1000
  )
}
