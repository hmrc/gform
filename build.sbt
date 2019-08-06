import Dependencies.appDependencies
import com.lucidchart.sbt.scalafmt.ScalafmtCorePlugin.autoImport._
import play.sbt.PlayImport.PlayKeys
import play.sbt.routes.RoutesKeys.routesImport
import sbt.Keys.{ resolvers, _ }
import sbt.Tests.{ Group, SubProcess }
import sbt._
import uk.gov.hmrc.DefaultBuildSettings.{ addTestReportOption, defaultSettings, scalaSettings }
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings
import uk.gov.hmrc.versioning.SbtGitVersioning
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
import uk.gov.hmrc.{ SbtArtifactory, SbtAutoBuildPlugin }

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    ScoverageKeys.coverageExcludedPackages := """uk.gov.hmrc.BuildInfo;._.Routes;._.RoutesPrefix;._Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;._.Reverse[^.]*""",
    ScoverageKeys.coverageMinimum := 80.00,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true,
    parallelExecution in Test := false
  )
}

lazy val microservice = (project in file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .settings(
    name := "gform",
    organization := "uk.gov.hmrc",
    majorVersion := 0,
    PlayKeys.playDefaultPort := 9196,
    scalaSettings,
    publishingSettings,
    defaultSettings(),
    scalafmtOnCompile := true,
    scalaVersion := "2.11.12",
    libraryDependencies ++= appDependencies,
    evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false),
    routesImport ++= Seq(
      "uk.gov.hmrc.auth.core.AffinityGroup",
      "uk.gov.hmrc.gform.sharedmodel.ValueClassBinder._",
      "uk.gov.hmrc.gform.sharedmodel._",
      "uk.gov.hmrc.gform.sharedmodel.form._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._"
    ),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
      "ofsted-notify-java-client" at "https://dl.bintray.com/gov-uk-notify/maven/"
    ),
    scalacOptions ++= Seq(
      //        "-Xfatal-warnings",
      "-Xlint:-missing-interpolator,_",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-dead-code",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:higherKinds"
    )
  )
  .configs(IntegrationTest)
  .settings(
    inConfig(IntegrationTest)(Defaults.itSettings),
    inConfig(IntegrationTest)(scalafmtCoreSettings),
    Keys.fork in IntegrationTest := false,
    unmanagedSourceDirectories in IntegrationTest := { (baseDirectory in IntegrationTest)(base => Seq(base / "it")) }.value,
    addTestReportOption(IntegrationTest, "int-test-reports"),
    testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
    parallelExecution in IntegrationTest := false
  )

def oneForkedJvmPerTest(tests: Seq[TestDefinition]): Seq[Group] =
  tests map { test =>
    Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
  }
