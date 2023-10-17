import play.core.PlayVersion
import sbt._
object Dependencies {

  val appName = "gform"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.2.0"
  val hmrcMongoVersion = "0.74.0"
  val bootstrapVersion = "7.15.0"
  val json4sVersion = "3.6.12"
  val circeVersion = "0.14.5"

  val compile = Seq(
    "com.github.pureconfig"        %% "pureconfig"                        % "0.17.2",
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-work-item-repo-play-28" % hmrcMongoVersion,
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-28"         % bootstrapVersion,
    "uk.gov.hmrc"                  %% "domain"                            % "8.2.0-play-28",
    "org.scala-lang.modules"       %% "scala-parser-combinators"          % "1.1.2",
    "org.julienrf"                 %% "play-json-derived-codecs"          % "10.1.0",
    "com.dripower"                 %% "play-circe"                        % "2814.1",
    "org.typelevel"                %% "cats-core"                         % "2.9.0",
    "org.typelevel"                %% "case-insensitive"                  % "1.3.0",
    "com.openhtmltopdf"             % "openhtmltopdf-pdfbox"              % "1.0.10",
    "com.chuusai"                  %% "shapeless"                         % "2.3.10",
    "org.scala-graph"              %% "graph-core"                        % "1.13.6",
    "com.github.jknack"             % "handlebars"                        % handlebarsVersion,
    "com.github.jknack"             % "handlebars-jackson2"               % handlebarsVersion,
    "com.softwaremill.quicklens"   %% "quicklens"                         % "1.9.1",
    "uk.gov.service.notify"         % "notifications-java-client"         % "3.19.1-RELEASE",
    "org.apache.commons"            % "commons-text"                      % "1.10.0",
    "uk.gov.hmrc.objectstore"      %% "object-store-client-play-28"       % "1.1.0",
    "io.circe"                     %% "circe-core"                        % circeVersion,
    "io.circe"                     %% "circe-generic"                     % circeVersion,
    "io.circe"                     %% "circe-parser"                      % circeVersion,
    "io.circe"                     %% "circe-literal"                     % circeVersion,
    "io.circe"                     %% "circe-json-schema"                 % "0.2.0",
    "com.github.tototoshi"         %% "scala-csv"                         % "1.3.10",
    "org.json4s"                   %% "json4s-core"                       % json4sVersion,
    "org.json4s"                   %% "json4s-native"                     % json4sVersion,
    "org.json4s"                   %% "json4s-xml"                        % json4sVersion,
    "com.fasterxml.jackson.module" %% "jackson-module-scala"              % "2.12.2"
  )

  def test(scope: String = "test,it") = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-28" % bootstrapVersion    % scope,
    "org.scalatestplus"      %% "scalacheck-1-14"        % "3.2.2.0"           % scope,
    "org.pegdown"             % "pegdown"                % "1.6.0"             % scope,
    "com.typesafe.play"      %% "play-test"              % PlayVersion.current % scope,
    "org.scalamock"          %% "scalamock"              % "5.2.0"             % scope,
    "org.scalatest"          %% "scalatest"              % "3.2.15"            % scope,
    "org.scalatestplus.play" %% "scalatestplus-play"     % "5.1.0"             % scope,
    "org.jsoup"               % "jsoup"                  % "1.15.4"            % scope,
    //"com.github.tomakehurst"  % "wiremock-jre8"               % "2.27.1"            % scope,
    "com.vladsch.flexmark" % "flexmark-all"            % "0.64.0"         % scope,
    "org.scalameta"       %% "munit"                   % "0.7.29"         % scope,
    "uk.gov.hmrc.mongo"   %% "hmrc-mongo-test-play-28" % hmrcMongoVersion % scope,
    "com.storm-enroute"   %% "scalameter"              % "0.21"           % scope,
    "com.typesafe"        %% "ssl-config-core"         % "0.6.1"          % scope // # Needed for it:test to run on JDK 17
  )
}
