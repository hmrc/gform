package uk.gov.hmrc.bforms.controllers

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import play.api.mvc.Action
import uk.gov.hmrc.bforms.services.{RetrieveService, SaveService}
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.Future

class SaveAndRetrieveController(val messagesApi: MessagesApi) extends SaveAndRetrieveHelperController {

  def saveForm = saveFormData

  def retrieveForm(registrationNumber : String) = retrieveFormData(registrationNumber)

}

trait SaveAndRetrieveHelperController extends BaseController with I18nSupport {

  def saveFormData():Action[JsValue] = Action.async(parse.json) { implicit request =>
    val messages = messagesApi.preferred(request)
    request.body.validate match {
      case JsSuccess(req, _) =>
        SaveService.save(req).map {
            case Left(x) => Ok(x)
            case Right(()) => Ok("Successful")
        }
      case JsError(jsonErrors) =>
        val response = jsonErrors match {
          case _ => Ok("Errors")
        }
        Future.successful(response)
    }
  }

  def retrieveFormData(registrationNumber : String) = Action.async { implicit request =>
    RetrieveService.retrieve(registrationNumber).map{
      case Left(x) => Ok(x)
      case Right(()) => BadRequest(s"No saved data for this $registrationNumber number")
    }
  }
}


