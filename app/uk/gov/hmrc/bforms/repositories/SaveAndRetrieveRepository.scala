package uk.gov.hmrc.bforms.repositories

import com.fasterxml.jackson.annotation.JsonValue
import play.api.libs.json.Json
import reactivemongo.api.DefaultDB
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.Future

class SaveAndRetrieveRepositoryImpl(implicit mongo: () => DefaultDB ) extends ReactiveRepository[Map[String, String], BSONObjectID]("Save_And_Retrieve", mongo, Json.format[Map[String, String]]) with SaveAndRetrieveRepository {

  def save(form:Map[String, String]) : Future[Either[String, Unit]] = {
    insert(form).map {
      case x if x.ok =>
        Right(())
      case x =>
        Left(x.message)
    }
  }

  def retrieve(registrationNumber:String): Future[List[Map[String, String]]] = {
    find("registratioNumber" -> registrationNumber)
  }

}

trait SaveAndRetrieveRepository {

  def save(form : Map[String, String]) : Future[Either[String, Unit]]

  def retrieve(registrationNumber : String) : Future[List[Map[String, String]]]

}
