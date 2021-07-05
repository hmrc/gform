package uk.gov.hmrc.gform.it

import org.apache.commons.lang3.RandomStringUtils
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import uk.gov.hmrc.gform.formmetadata.FormMetadata
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }
import scala.concurrent.ExecutionContext.Implicits.global

trait MongoDBSupport {

  val mongoDbName: String = s"test-${RandomStringUtils.randomNumeric(5)}-${getClass.getSimpleName}"
  val mongoDBURI: String = s"mongodb://localhost:27017/$mongoDbName"
  val mongoSettings: Map[String, String] = Map(
    "mongodb.uri" -> mongoDBURI
  )
  val mongoComponent: MongoComponent = MongoComponent(mongoDBURI)

  val formTemplateRepo: PlayMongoRepository[FormTemplate] =
    new PlayMongoRepository[FormTemplate](
      mongoComponent = mongoComponent,
      collectionName = "formTemplate",
      domainFormat = FormTemplate.format,
      indexes = Seq.empty
    )

  val formTemplateRawRepo: PlayMongoRepository[FormTemplateRaw] =
    new PlayMongoRepository[FormTemplateRaw](
      mongoComponent = mongoComponent,
      collectionName = "formTemplateRaw",
      domainFormat = FormTemplateRaw.format,
      indexes = Seq.empty
    )

  val formMetadataRepo: PlayMongoRepository[FormMetadata] =
    new PlayMongoRepository[FormMetadata](
      mongoComponent = mongoComponent,
      collectionName = "formMetadata",
      domainFormat = FormMetadata.format,
      indexes = Seq.empty
    )
}
