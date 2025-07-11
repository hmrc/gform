import play.core.PlayVersion
import sbt.*
object Dependencies {

  val appName = "gform"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"
  val handlebarsVersion = "4.2.1"
  val hmrcMongoVersion = "2.6.0"
  val bootstrapVersion = "9.13.0"
  val json4sVersion = "4.0.4"
  val circeVersion = "0.14.5"

  val compile = Seq(
    "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
    "com.github.pureconfig"        %% "pureconfig"                        % "0.17.2",
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-work-item-repo-play-30" % hmrcMongoVersion,
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-30"         % bootstrapVersion,
    "uk.gov.hmrc"                  %% "domain-play-30"                    % "9.0.0",
    "uk.gov.hmrc"                  %% "reactive-circuit-breaker"          % "5.0.0",
    "org.julienrf"                 %% "play-json-derived-codecs"          % "11.0.0",
    "com.dripower"                 %% "play-circe"                        % "3014.1",
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
    "uk.gov.hmrc.objectstore"      %% "object-store-client-play-30"       % "1.4.0",
    "io.circe"                     %% "circe-core"                        % circeVersion,
    "io.circe"                     %% "circe-generic"                     % circeVersion,
    "io.circe"                     %% "circe-parser"                      % circeVersion,
    "io.circe"                     %% "circe-literal"                     % circeVersion,
    "io.circe"                     %% "circe-json-schema"                 % "0.2.0",
    "com.github.tototoshi"         %% "scala-csv"                         % "1.3.10",
    "org.json4s"                   %% "json4s-core"                       % json4sVersion,
    "org.json4s"                   %% "json4s-native"                     % json4sVersion,
    "org.json4s"                   %% "json4s-xml"                        % json4sVersion,
    "com.fasterxml.jackson.module" %% "jackson-module-scala"              % "2.12.2",
    "io.github.samueleresca"       %% "pekko-quartz-scheduler"            % "1.2.0-pekko-1.0.x",
    "org.apache.xmlgraphics"        % "fop"                               % "2.9",
    "org.jsoup"                     % "jsoup"                             % "1.18.1",
    "org.apache.poi"                % "poi-ooxml"                         % "5.2.4",
    "org.bouncycastle"              % "bcprov-jdk18on"                    % "1.80",
    "org.bouncycastle"              % "bcpg-jdk18on"                      % "1.80",
    "org.bouncycastle"              % "bcpkix-jdk18on"                    % "1.80"
  )

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-30" % bootstrapVersion    % scope,
    "org.scalatestplus"      %% "scalacheck-1-14"        % "3.2.2.0"           % scope,
    "org.playframework"      %% "play-test"              % "3.0.2"             % scope,
    "org.playframework"      %% "play-filters-helpers"   % "3.0.2"             % scope,
    "org.scalamock"          %% "scalamock"              % "5.2.0"             % scope,
    "org.scalatest"          %% "scalatest"              % "3.2.15"            % scope,
    "org.scalatestplus.play" %% "scalatestplus-play"     % "5.1.0"             % scope,
    "org.jsoup"               % "jsoup"                  % "1.15.4"            % scope,
    //"com.github.tomakehurst"  % "wiremock-jre8"               % "2.27.1"            % scope,
    "com.vladsch.flexmark" % "flexmark-all"            % "0.64.0"         % scope,
    "org.scalameta"       %% "munit"                   % "1.1.0"          % scope,
    "uk.gov.hmrc.mongo"   %% "hmrc-mongo-test-play-30" % hmrcMongoVersion % scope,
    "com.storm-enroute"   %% "scalameter"              % "0.21"           % scope,
    "com.typesafe"        %% "ssl-config-core"         % "0.6.1"          % scope // # Needed for it:test to run on JDK 17
  )
}
