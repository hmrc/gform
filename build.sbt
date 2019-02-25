import Dependencies.appDependencies
import sbt.Tests.{ Group, SubProcess }
import uk.gov.hmrc.DefaultBuildSettings.{ addTestReportOption, defaultSettings, scalaSettings }
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

lazy val playSettings: Seq[Setting[_]] = Seq.empty

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    // Semicolon-separated list of regexs matching classes to exclude
    ScoverageKeys.coverageExcludedPackages := """uk.gov.hmrc.BuildInfo;._.Routes;._.RoutesPrefix;._Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;._.Reverse[^.]*""",
    ScoverageKeys.coverageMinimum := 80.00,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true,
    parallelExecution in Test := false
  )
}

name := "gform"
organization := "uk.gov.hmrc"
majorVersion := 0
PlayKeys.playDefaultPort := 9196
playSettings
scalaSettings
publishingSettings
defaultSettings()

enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)

scalafmtOnCompile := true
scalaVersion := "2.11.12"
libraryDependencies ++= appDependencies
evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false)

routesImport ++= Seq(
  "uk.gov.hmrc.gform.sharedmodel.ValueClassBinder._",
  "uk.gov.hmrc.gform.sharedmodel._",
  "uk.gov.hmrc.gform.sharedmodel.form._",
  "uk.gov.hmrc.gform.sharedmodel.formtemplate._"
)
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

Keys.fork in IntegrationTest := false
unmanagedSourceDirectories in IntegrationTest := Seq((baseDirectory in IntegrationTest).value / "it")
addTestReportOption(IntegrationTest, "int-test-reports")
testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value)
parallelExecution in IntegrationTest := false

configs(IntegrationTest)
inConfig(IntegrationTest)(Defaults.itSettings)

def oneForkedJvmPerTest(tests: Seq[TestDefinition]): Seq[Group] =
  tests map { test =>
    Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
  }

resolvers ++= Seq(
  Resolver.jcenterRepo,
  "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"
)
