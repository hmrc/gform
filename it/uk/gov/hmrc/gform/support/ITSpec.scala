package uk.gov.hmrc.gform.support

import org.scalatestplus.play.{BaseOneServerPerTest, FakeApplicationFactory}
import play.api.ApplicationLoader.Context
import play.api.libs.ws.WSClient
import play.api.{Application, Configuration, Environment}
import play.core.DefaultWebCommands
import uk.gov.hmrc.gform.ApplicationModule
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.HeaderCarrier

/**
  * This spec provides running play application for every test.
  */
trait ITSpec extends ITSpecBase with BaseOneServerPerTest with /*TODO MongoSpecSupport with */FakeApplicationFactory {

  override def fakeApplication(): Application = application

  lazy val wsclient = new WSHttp

  private lazy val mongoDbName: String = "test-" + this.getClass.getSimpleName
  private lazy val env: Environment = Environment.simple()
  private lazy val configurationOverridings = Map(
    "mongodb.uri" -> s"mongodb://localhost:27017/$mongoDbName",
    "auditing.enabled" -> "false",
    "feature.basicAuthEnabled" -> "true"
  )
  private lazy val context: Context = Context(
    environment = env,
    sourceMapper = None,
    webCommands = new DefaultWebCommands(),
    initialConfiguration = Configuration.load(env)
  )
  private lazy val applicationModule = new ApplicationModule(context) {
    override lazy val httpFilters = Nil
  }
  private lazy val application = applicationModule.application

  implicit lazy val hc = HeaderCarrier()

  lazy val baseUrl = s"http://localhost:${port}"
}
