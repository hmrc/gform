package uk.gov.hmrc.gform.it

import uk.gov.hmrc.gform.MongoComponentSupport
import uk.gov.hmrc.gform.formmetadata.FormMetadata
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateRaw }
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.cache.CacheIdType.SimpleCacheId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
// import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
// import uk.gov.hmrc.mongo.MongoUtils

// import java.util.concurrent.TimeUnit

trait MongoDBSupport extends MongoComponentSupport {

  val mongoSettings: Map[String, String] = Map(
    "mongodb.uri" -> mongoDBURI
  )
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

  val formCacheRepository: MongoCacheRepository[String] =
    new MongoCacheRepository[String](
      mongoComponent,
      "forms",
      true,
      1.days,
      new CurrentTimestampSupport(),
      SimpleCacheId
    ) {
      override def ensureIndexes: Future[Seq[String]] = Future.successful(Seq())
    }
}
