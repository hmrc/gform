import play.core.PlayVersion
import play.sbt.PlayImport.ws
import sbt._
object Dependencies {

  val appName = "gform"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.2.0"
  val hmrcMongoVersion = "0.50.0"

  val compile = Seq(
    ws,
    "com.github.pureconfig"      %% "pureconfig"                % "0.14.0",
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-play-27"        % hmrcMongoVersion,
    "uk.gov.hmrc"                %% "http-caching-client"       % "9.2.0-play-27",
    "uk.gov.hmrc"                %% "bootstrap-backend-play-27" % "3.2.0",
    "uk.gov.hmrc"                %% "domain"                    % "5.10.0-play-27",
    "uk.gov.hmrc"                %% "auth-client"               % "3.0.0-play-27", // we need AffinityGroup type to stay in sync with frontend
    "com.typesafe.play"          %% "play-json"                 % "2.7.4",
    "com.codecommit"             %% "parseback-core"            % parsebackVersion,
    "com.codecommit"             %% "parseback-cats"            % parsebackVersion,
    "org.julienrf"               %% "play-json-derived-codecs"  % "4.0.1", //upgrading this has caused play-json comparability issue
    "org.typelevel"              %% "cats-core"                 % "1.6.0",
    "com.openhtmltopdf"           % "openhtmltopdf-pdfbox"      % "1.0.6",
    "com.chuusai"                %% "shapeless"                 % "2.3.3",
    "com.github.mpilquist"       %% "simulacrum"                % "0.15.0",
    "org.scala-graph"            %% "graph-core"                % "1.12.5",
    "com.github.jknack"           % "handlebars"                % handlebarsVersion,
    "com.github.jknack"           % "handlebars-jackson2"       % handlebarsVersion,
    "com.softwaremill.quicklens" %% "quicklens"                 % "1.4.11",
    "uk.gov.service.notify"       % "notifications-java-client" % "3.17.0-RELEASE",
    "org.apache.commons"          % "commons-text"              % "1.9"
  )

  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc"            %% "service-integration-test"    % "0.13.0-play-27"    % scope,
    "org.scalacheck"         %% "scalacheck"                  % "1.14.3"            % scope,
    "org.pegdown"             % "pegdown"                     % "1.6.0"             % scope,
    "com.typesafe.play"      %% "play-test"                   % PlayVersion.current % scope,
    "org.scalamock"          %% "scalamock-scalatest-support" % "3.6.0"             % scope,
    "org.scalatestplus.play" %% "scalatestplus-play"          % "4.0.3"             % scope,
    "org.jsoup"               % "jsoup"                       % "1.11.3"            % scope,
    "com.github.tomakehurst"  % "wiremock-jre8"               % "2.27.1"            % scope,
    "org.scalameta"          %% "munit"                       % "0.7.22"            % scope,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-27"     % hmrcMongoVersion    % scope,
    "com.storm-enroute"      %% "scalameter"                  % "0.19"              % scope
  )
}
