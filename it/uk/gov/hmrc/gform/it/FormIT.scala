package uk.gov.hmrc.gform.it

import akka.http.scaladsl.model.StatusCodes
import com.mongodb.{ BasicDBObject, ReadPreference }
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.it.sample.{ FormDataSample, FormTemplateSample, QueryParamsSample }
import uk.gov.hmrc.gform.it.wiremock.FileUploadServiceStubs
import uk.gov.hmrc.gform.save4later.EncryptedFormFormat
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData.Plain
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.mongo.cache.DataKey

import scala.concurrent.ExecutionContext.Implicits.global
import java.time.{ Instant, ZoneOffset }

class FormIT
    extends ITSpec with FormTemplateSample with FormDataSample with QueryParamsSample with FileUploadServiceStubs {
  override implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(500, Millis))

  override protected def afterEach(): Unit = {
    super.afterEach()
    formTemplateRepo.collection.deleteMany(new BasicDBObject()).toFuture().futureValue
    formTemplateRawRepo.collection.deleteMany(new BasicDBObject()).toFuture().futureValue
    ()
  }

  "new form" should "return create a new instance of form" in {
    val startInstant = Instant.now()
    createEnvelopeStub()

    Given("I have a form template")
    post(basicFormTemplate().toString).to("/formtemplates").send()

    When("I request a new form for the template")
    val newForm = post(queryParamsSample.toString).to("/new-form/basic/123").send()

    Then("The response should be OK")
    newForm.status shouldBe StatusCodes.OK.intValue
    Json.parse(newForm.body).as[FormIdData] shouldBe Plain(UserId("123"), FormTemplateId("basic"))

    And("The new form should be saved in forms collection")
    assertForm(startInstant)

    And("Saved in form metadata repo")
    assertFormMetadata(startInstant)
  }

  "update form" should "update form data in forms collection" in {
    val startInstant = Instant.now()
    createEnvelopeStub()

    Given("I have setup a form instance")
    post(basicFormTemplate().toString).to("/formtemplates").send()
    post(queryParamsSample.toString).to("/new-form/basic/123").send()
    wireMockServer.resetRequests()

    And("I update the form instance with new data")
    val response = put(formDataSample.toString).to("/forms/123/basic").send()

    Then("The response should be NoContent")
    response.status shouldBe StatusCodes.NoContent.intValue

    And("The new form should updated in forms collection")
    assertForm(startInstant, Seq(FormField(FormComponentId("textField1"), "textField1Value")), Set(0))

    assertFormMetadata(startInstant)
  }

  private def assertForm(
    startInstant: Instant,
    formFields: Seq[FormField] = Seq.empty,
    visitIndex: Set[Int] = Set.empty
  ): Unit = {
    implicit val formatFormEncrypted: Format[Form] = EncryptedFormFormat.formatEncrypted(jsonCrypto)
    val formDataKey: DataKey[Form] = DataKey("form")

    val form = formCacheRepository.get[Form]("123-basic")(formDataKey).futureValue.get
    form._id shouldBe FormId("123-basic")
    form.envelopeId shouldBe EnvelopeId("some-envelope-id")
    form.userId shouldBe UserId("123")
    form.formTemplateId shouldBe FormTemplateId("basic")
    form.formData.fields shouldBe formFields
    form.status shouldBe InProgress
    form.visitsIndex shouldBe VisitIndex.Classic(visitIndex)
    form.thirdPartyData shouldBe ThirdPartyData.empty
    form.envelopeExpiryDate.get.ldt.toInstant(ZoneOffset.UTC).isAfter(startInstant) shouldBe true
    form.componentIdToFileId shouldBe FormComponentIdToFileIdMapping.empty
    ()
  }

  private def assertFormMetadata(startInstant: Instant): Unit = {
    val formMetadatas = formMetadataRepo.collection
      .withReadPreference(ReadPreference.secondaryPreferred)
      .find()
      .toFuture()
      .map(_.toList)
      .futureValue
    formMetadatas.size shouldBe 1
    formMetadatas.head._id shouldBe FormId("123-basic")
    formMetadatas.head.userId shouldBe UserId("123")
    formMetadatas.head.formTemplateId shouldBe FormTemplateId("basic")
    formMetadatas.head.submissionRef shouldBe None
    formMetadatas.head.parentFormSubmissionRefs shouldBe List.empty
    formMetadatas.head.createdAt.isAfter(startInstant) shouldBe true
    formMetadatas.head.updatedAt.isAfter(startInstant) shouldBe true
    ()
  }
}
