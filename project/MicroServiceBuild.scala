import sbt._
import play.sbt.PlayImport._
import play.core.PlayVersion

object MicroServiceBuild extends Build with MicroService {

  val appName = "gform"

  override lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.1.2"
  
  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "play-reactivemongo" % "6.4.0",
    "com.github.pureconfig" %% "pureconfig" % "0.10.2",
    "uk.gov.hmrc" %% "http-caching-client" % "8.0.0",
    "uk.gov.hmrc" %% "microservice-bootstrap" % "10.3.0",
    "uk.gov.hmrc" %% "domain" % "5.3.0",
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "5.0.0",
    "org.typelevel" %% "cats-core" % "1.6.0",
    "org.apache.pdfbox" % "pdfbox" % "2.0.13",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.github.mpilquist" %% "simulacrum" % "0.15.0",
    "uk.gov.hmrc" %% "auth-client" % "2.19.0-play-25", // we need AffinityGroup type to stay in sync with frontend
    "org.scala-graph" %% "graph-core" % "1.12.5",
    "com.github.jknack" % "handlebars" % handlebarsVersion,
    "com.github.jknack" % "handlebars-jackson2" % handlebarsVersion
  )

  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "3.4.0-play-25" % scope,
    "org.scalatest" %% "scalatest" % "3.0.5" % scope,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "org.jsoup" % "jsoup" % "1.11.3" % scope
  )
}
