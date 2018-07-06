import sbt._
import play.sbt.PlayImport._
import play.core.PlayVersion
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning

object MicroServiceBuild extends Build with MicroService {

  val appName = "gform"

  override lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "play-reactivemongo" % "5.2.0",
    "com.github.pureconfig" %% "pureconfig" % "0.8.0",
    "uk.gov.hmrc" %% "http-caching-client" % "7.1.0",
    "uk.gov.hmrc" %% "microservice-bootstrap" % "6.18.0",
    "uk.gov.hmrc" %% "play-url-binders" % "2.1.0",
    "uk.gov.hmrc" %% "domain" % "5.1.0",
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "3.3",
    "org.typelevel" %% "cats" % "0.9.0",
    "org.apache.pdfbox" % "pdfbox" % "2.0.7",
    "io.github.cloudify" %% "spdf" % "1.4.0",
    "com.chuusai" %% "shapeless" % "2.3.2",
    "com.github.pureconfig" %% "pureconfig" % "0.7.2",
    "com.github.mpilquist" %% "simulacrum" % "0.11.0",
    "uk.gov.hmrc" %% "auth-client" % "2.6.0" // we need AffinityGroup type to stay in sync with frontend
  )


  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "3.0.0" % scope,
    "org.scalatest" %% "scalatest" % "3.0.4" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "uk.gov.hmrc" %% "reactivemongo-test" % "2.0.0" % scope,
    "org.jsoup" % "jsoup" % "1.10.3" % scope
  )

}
