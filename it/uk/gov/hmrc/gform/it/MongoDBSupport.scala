package uk.gov.hmrc.gform.it

import org.apache.commons.lang3.RandomStringUtils
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.gform.formmetadata.FormMetadata
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import uk.gov.hmrc.mongo.{ MongoConnector, ReactiveRepository }

trait MongoDBSupport {

  val mongoDbName: String = s"test-${RandomStringUtils.randomNumeric(5)}-${getClass.getSimpleName}"
  val mongoDBURI: String = s"mongodb://localhost:27017/$mongoDbName"
  val mongoSettings: Map[String, String] = Map(
    "mongodb.uri" -> mongoDBURI
  )

  implicit val mongoConnector: MongoConnector = MongoConnector(mongoDBURI, failoverStrategy = None)

  val formTemplateRepo: ReactiveRepository[FormTemplate, BSONObjectID] =
    new ReactiveRepository[FormTemplate, BSONObjectID](
      collectionName = "formTemplate",
      mongo = mongoConnector.db,
      domainFormat = FormTemplate.format,
      idFormat = ReactiveMongoFormats.objectIdFormats) {}

  val formTemplateRawRepo: ReactiveRepository[FormTemplateRaw, BSONObjectID] =
    new ReactiveRepository[FormTemplateRaw, BSONObjectID](
      collectionName = "formTemplateRaw",
      mongo = mongoConnector.db,
      domainFormat = FormTemplateRaw.format,
      idFormat = ReactiveMongoFormats.objectIdFormats) {}

  val formMetadataRepo: ReactiveRepository[FormMetadata, BSONObjectID] =
    new ReactiveRepository[FormMetadata, BSONObjectID](
      collectionName = "formMetadata",
      mongo = mongoConnector.db,
      domainFormat = FormMetadata.format,
      idFormat = ReactiveMongoFormats.objectIdFormats) {}
}
