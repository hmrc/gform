package uk.gov.hmrc.gform.it

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.`extension`.responsetemplating.ResponseTemplateTransformer
import com.github.tomakehurst.wiremock.client.WireMock.configureFor
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.options
import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, GivenWhenThen, Matchers }
import org.scalatestplus.play.{ BaseOneServerPerSuite, FakeApplicationFactory }
import play.api.ApplicationLoader.Context
import play.api.libs.json.{ Json, Reads }
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import play.api.{ Application, Environment }
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig }
import uk.gov.hmrc.gform.ApplicationLoader

import scala.util.Random
import scala.collection.JavaConverters._

trait ITSpec
    extends MongoDBSupport with HTTPSupport with FlatSpecLike with GivenWhenThen with Matchers
    with BaseOneServerPerSuite with BeforeAndAfterAll with FakeApplicationFactory with ScalaFutures
    with BeforeAndAfterEach {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val wiremockPort: Int = 10000 + Random.nextInt(10000)
  val wireMockServer: WireMockServer = new WireMockServer(
    options().port(wiremockPort).extensions(new ResponseTemplateTransformer(false))
  )

  val settingsOverride: Map[String, String] = Map(
    "auditing.enabled"                              -> "false",
    "json.encryption.key"                           -> "fqpLDZ4sumDsekHkeEBlCA==",
    "json.encryption.previousKeys"                  -> "",
    "microservice.services.file-upload.port"        -> s"$wiremockPort",
    "microservice.services.file-upload.path-prefix" -> "",
    "microservice.services.save4later.port"         -> s"$wiremockPort"
  ) ++ mongoSettings

  override def fakeApplication(): Application = {
    val context =
      Context.create(environment = Environment.simple(), initialSettings = settingsOverride)
    new ApplicationLoader().load(context)
  }

  override protected def beforeAll(): Unit = {
    wireMockServer.start()
    configureFor("localhost", wiremockPort)
  }

  override protected def beforeEach(): Unit =
    wireMockServer.resetAll()

  override protected def afterAll(): Unit = {
    app.stop().futureValue
    wireMockServer.stop()
    mongoComponent.database.drop().toFuture().futureValue
    system.terminate()
    ()
  }

  def decryptAs[T: Reads](json: String) =
    Json.parse(jsonCrypto.decrypt(Crypted(Json.parse(json).toString())).value).as[T]

  implicit lazy val baseUrl: String = s"http://localhost:$port/gform"
  implicit val wsClient: StandaloneAhcWSClient = StandaloneAhcWSClient()

  val jsonCrypto =
    new CryptoWithKeysFromConfig(baseConfigKey = "json.encryption", ConfigFactory.parseMap(settingsOverride.asJava))
}
