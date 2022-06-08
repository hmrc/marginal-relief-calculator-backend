import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc" %% "bootstrap-backend-play-28" % "5.24.0",
    "org.typelevel" %% "cats-core" % "2.7.0",
    "org.julienrf" %% "play-json-derived-codecs" % "10.0.2"
  )

  val test = Seq(
    "uk.gov.hmrc" %% "bootstrap-test-play-28" % "5.24.0" % "test, it",
    "org.mockito" %% "mockito-scala-scalatest" % "1.17.7" % "test, it",
    "com.vladsch.flexmark" % "flexmark-all" % "0.36.8" % "test, it"
  )
}
