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

package uk.gov.hmrc.gform.testonly

import java.text.SimpleDateFormat
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import play.api.Logger
import play.api.libs.json._
import play.api.mvc._
import reactivemongo.api.DB
import reactivemongo.play.json.collection.JSONCollection
import uk.gov.hmrc.BuildInfo
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des._
import uk.gov.hmrc.gform.sharedmodel._

import scala.concurrent.Future

class TestOnlyController(mongo: () => DB, enrolmentConnector: EnrolmentConnector) extends BaseController {

  lazy val formTemplates = mongo().collection[JSONCollection]("formTemplate")
  def removeTemplates() = Action.async { implicit request =>
    println("purging mongo database ....")
    formTemplates.drop(failIfNotFound = false).map(_ => Results.Ok("Mongo purged")).recover {

      case e =>
        e.printStackTrace()
        Results.InternalServerError(e.toString)
    }
  }

  def buildInfo() = Action { r =>
    Results.Ok(Json.toJson(BuildInfo.toMap.mapValues(_.toString)))

  }

  case class User(id: String, postCode: String, countryCode: String)

  object User {
    val reads: Reads[User] = Json.format[User]
    val write: OWrites[User] = OWrites[User] { o =>
      getJson(o)
    }

    implicit val format = OFormat[User](reads, write)
  }

  def config() = Action { r =>
    val result: JsValue = Json.parse(ConfigFactory.load().root().render(ConfigRenderOptions.concise()))
    Results.Ok(result)
  }

  def getJson(user: User): JsObject =
    if (user.postCode.nonEmpty) {
      Json.obj(
        "verifiers" -> Json.arr(
          Json.obj("key" -> "NonUkCountryCode", "value" -> user.countryCode),
          Json.obj("key" -> "BusinessPostcode", "value" -> user.postCode))) //{"verifiers" : [{"key" : "NonUkCountryCode","value" : "GB"},{"key" : "BusinessPostcode","value" : "E499OL"}]}
    } else {
      Json.obj("verifiers" -> Json.arr(Json.obj("key" -> "NonUkCountryCode", "value" -> user.countryCode)))
    }

  def upload = Action.async(parse.json[User]) { implicit request =>
    val user: User = request.body
    enrolmentConnector.upload(user.id, Json.toJson(user)).map(_ => NoContent)
  }

  def deEnrolUser(userId: String) = Action.async(parse.json[User]) { implicit request =>
    val user = request.body
    enrolmentConnector.deEnrol(userId, user.id).map(x => Ok(x.body))
  }

  def delete = Action.async(parse.json[User]) { implicit request =>
    val user = request.body
    enrolmentConnector.removeUnallocated(user.id).map(_ => NoContent)
  }

  def testValidatorStub(utr: String) = Action.async { implicit request =>
    Logger.info(s"testValidatorStub, ${loggingHelpers.cleanHeaders(request.headers)}")
    if (utr.startsWith("1")) {
      Future.successful(Ok(Json.toJson(AddressDes("BN12 4XL"))))
    } else
      Future.successful(Ok(Json.toJson(AddressDes("ANYTHING"))))
  }

  val stringToDate = new SimpleDateFormat("yyyy-MM-dd")

  def testGetTaxPeriods(idType: String, idNumber: String, regimeType: String) = Action.async { implicit request =>
    Logger.info(s"testGetTaxStub, ${loggingHelpers.cleanHeaders(request.headers)}")
    if (idType == "nino")
      Future.successful(
        Ok(
          Json.toJson(
            new Obligation(List(new TaxPeriodDes(
              new Identification("ITSA", "PB910220A", "nino"),
              List(
                new ObligationDetail("O", "2019-05-23", "2019-10-12", "2019-04-11", "2019-11-21", "#002"),
                new ObligationDetail("O", "2019-06-24", s"2019-09-24", "2019-03-24", "2019-07-01", "#001")
              )
            )))
          )))
    else {
      Future.successful(BadRequest("idType wasn't nino"))
    }
  }
}
