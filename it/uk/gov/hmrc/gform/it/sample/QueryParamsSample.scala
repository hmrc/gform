package uk.gov.hmrc.gform.it.sample

import play.api.libs.json.Json

trait QueryParamsSample {
  val queryParamsSample = Json.parse("""{ "params": {} }""")
}
