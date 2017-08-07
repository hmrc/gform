package uk.gov.hmrc.gform.fileupload

import play.api.libs.json.{JsObject, JsResult, JsSuccess, Json}
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.Form

class RouteEnvelopeRequestSpec extends Spec {

  behavior of "RouteEnvelopeRequest"

  it should "render json" in new ExampleData {

    val jsObject = Json.obj(
      "envelopeId" -> "b66c5979-e885-49cd-9281-c7f42ce6b307",
      "application" -> "GFORM",
      "destination" -> "DMS"
    )

    val writtenJsObject: JsObject = RouteEnvelopeRequest.format.writes(routeEnvelopeRequest)
    println(Json.prettyPrint (writtenJsObject))
    jsObject shouldBe writtenJsObject
    RouteEnvelopeRequest.format.reads(jsObject) should be(JsSuccess(routeEnvelopeRequest))
  }


}
