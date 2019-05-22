import play.core.PlayVersion
import play.sbt.PlayImport.ws
import sbt._
object Dependencies {

  val appName = "gform"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.1.2"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "play-reactivemongo" % "6.4.0",
    "com.github.pureconfig" %% "pureconfig" % "0.10.2",
    "uk.gov.hmrc" %% "http-caching-client" % "8.0.0",
    "uk.gov.hmrc" %% "microservice-bootstrap" % "10.6.0",
    "uk.gov.hmrc" %% "domain" % "5.3.0",
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "3.3", //upgrading this has caused play-json comparability issue
    "org.typelevel" %% "cats-core" % "1.6.0",
    "org.apache.pdfbox" % "pdfbox" % "2.0.13",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.github.mpilquist" %% "simulacrum" % "0.15.0",
    "uk.gov.hmrc" %% "auth-client" % "2.19.0-play-25", // we need AffinityGroup type to stay in sync with frontend
    "org.scala-graph" %% "graph-core" % "1.12.5",
    "com.github.jknack" % "handlebars" % handlebarsVersion,
    "com.github.jknack" % "handlebars-jackson2" % handlebarsVersion,
    "com.itv" %% "scalapact-argonaut-6-2"  % "2.2.5",
    "com.itv" %% "scalapact-http4s-0-16-2" % "2.2.5",
    "com.softwaremill.quicklens" %% "quicklens" % "1.4.11",
    "com.softwaremill.sttp" %% "core" % "1.5.17",
    "uk.gov.service.notify" % "notifications-java-client" % "3.14.2-RELEASE"
  )


  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "3.5.0-play-25" % scope,
    "org.scalatest" %% "scalatest" % "3.0.5" % scope,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "org.jsoup" % "jsoup" % "1.11.3" % scope,
    "com.itv" %% "scalapact-circe-0-9"     % "2.2.5" % scope,
    "com.itv" %% "scalapact-http4s-0-18-0" % "2.2.5" % scope,
    "com.itv" %% "scalapact-scalatest"     % "2.2.5" % scope,
    "com.softwaremill.sttp" %% "core" % "1.5.15" % scope
  )
}
