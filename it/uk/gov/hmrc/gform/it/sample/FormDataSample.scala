package uk.gov.hmrc.gform.it.sample

import play.api.libs.json.Json

trait FormDataSample {
  val formDataSample = Json.parse("""{
                                    | "formData": {
                                    |   "fields" : [
                                    |     {
                                    |       "id": "textField1",
                                    |       "value": "textField1Value"
                                    |     }
                                    |   ]
                                    | },
                                    | "formStatus": {
                                    |   "InProgress": {}
                                    | },
                                    | "visitsIndex": {
                                    |   "visitsIndex": [ 0 ]
                                    | },
                                    | "thirdPartyData": {
                                    |   "obligations": {
                                    |     "NotChecked": {}
                                    |   },
                                    |   "emailVerification": {},
                                    |   "queryParams": {
                                    |     "params": {}
                                    |   },
                                    |   "booleanExprCache": {
                                    |     "mapping": {}
                                    |   }
                                    | }
                                    |}""".stripMargin)
}
