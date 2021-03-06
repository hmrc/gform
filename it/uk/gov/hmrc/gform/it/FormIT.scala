package uk.gov.hmrc.gform.it

import akka.http.scaladsl.model.StatusCodes
import com.mongodb.{ BasicDBObject, ReadPreference }
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.it.sample.{ FormDataSample, FormTemplateSample, QueryParamsSample }
import uk.gov.hmrc.gform.it.wiremock.{ FileUploadServiceStubs, Save4LaterServiceStubs }
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData.Plain
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.mongo.cache.DataKey

import scala.concurrent.ExecutionContext.Implicits.global
import java.time.{ Instant, ZoneOffset }

class FormIT
    extends ITSpec with FormTemplateSample with FormDataSample with QueryParamsSample with Save4LaterServiceStubs
    with FileUploadServiceStubs {
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
    save4laterPUTStub("123", "BASIC")

    Given("I have a form template")
    post(basicFormTemplate.toString).to("/formtemplates").send()

    When("I request a new form for the template")
    val newForm = post(queryParamsSample.toString).to("/new-form/BASIC/123").send()

    Then("The response should be OK")
    newForm.status shouldBe StatusCodes.OK.intValue
    Json.parse(newForm.body).as[FormIdData] shouldBe Plain(UserId("123"), FormTemplateId("BASIC"))

    And("The new form should be saved in forms collection")
    assertForm(startInstant)

    And("Saved in form metadata repo")
    assertFormMetadata(startInstant)
  }

  "update form" should "update form data in save4later service" in {
    val startInstant = Instant.now()
    createEnvelopeStub()
    save4laterPUTStub("123", "BASIC")
    save4laterGETStub("123", "BASIC", FormData(Seq.empty), InProgress)

    Given("I have setup a form instance")
    post(basicFormTemplate.toString()).to("/formtemplates").send()
    post(queryParamsSample.toString).to("/new-form/BASIC/123").send()
    wireMockServer.resetRequests()

    And("I update the form instance with new data")
    val response = put(formDataSample.toString).to("/forms/123/BASIC").send()

    Then("The response should be NoContent")
    response.status shouldBe StatusCodes.NoContent.intValue

    And("The new form should updated in save4later service")
    assertForm(startInstant, Seq(FormField(FormComponentId("textField1"), "textField1Value")), Set(0))

    assertFormMetadata(startInstant)
  }

  private def assertForm(
    startInstant: Instant,
    formFields: Seq[FormField] = Seq.empty,
    visitIndex: Set[Int] = Set.empty
  ): Unit = {
    val encryptedForm = formCacheRepository.get("123-BASIC")(DataKey[String]("form")).futureValue.get
    val form = decryptAs[Form](encryptedForm)
    form._id shouldBe FormId("123-BASIC")
    form.envelopeId shouldBe EnvelopeId("some-envelope-id")
    form.userId shouldBe UserId("123")
    form.formTemplateId shouldBe FormTemplateId("BASIC")
    form.formData.fields shouldBe formFields
    form.status shouldBe InProgress
    form.visitsIndex shouldBe VisitIndex(visitIndex)
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
    formMetadatas.head._id shouldBe FormId("123-BASIC")
    formMetadatas.head.userId shouldBe UserId("123")
    formMetadatas.head.formTemplateId shouldBe FormTemplateId("BASIC")
    formMetadatas.head.submissionRef shouldBe None
    formMetadatas.head.parentFormSubmissionRefs shouldBe List.empty
    formMetadatas.head.createdAt.isAfter(startInstant) shouldBe true
    formMetadatas.head.updatedAt.isAfter(startInstant) shouldBe true
    ()
  }
}
